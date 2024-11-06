package com.veadan.folib.ws.task;

;
import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.ws.server.DistributionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.lang.reflect.Field;
import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * 任务处理器
 */
@Component
public class TaskProcessor {
    private static final Logger logger = LoggerFactory.getLogger(TaskProcessor.class);
    private final DistributionService distributionService;
    private final Sinks.Many<DistributionTask> sink;
    private final Scheduler ioScheduler;
    //@Value("${folib.promotion.thread:4}")
    private  int cpuCores;

    /**
     * TaskProcessor 构造函数
     * 初始化分发服务、任务执行器和任务触发器
     *
     * @param distributionService 注入的 DistributionService 实例
     */
    @Autowired
    public TaskProcessor(DistributionService distributionService) throws NoSuchFieldException, IllegalAccessException {

        cpuCores = SpringUtil.getProperty("folib.promotion.thread") == null ? 4 : Integer.parseInt(SpringUtil.getProperty("folib.promotion.thread"));
        this.distributionService = distributionService;
        //支持多个订阅者（多播），并且使用缓冲区来处理背压
        this.sink = Sinks.many().multicast().onBackpressureBuffer();

        //ToDo cpu核心数用于并发数量，并发数量固定后要考虑网络带宽，否则会出现网络带宽不足
        //cpuCores = Runtime.getRuntime().availableProcessors();
        //cpuCores = 4;
        // 配置一个适用于 IO 密集型任务的 Scheduler
        //ioScheduler = Schedulers.fromExecutor(executor);
        ioScheduler = Schedulers.boundedElastic();
        initializeTaskProcessing();
    }
    /**
     * 初始化任务处理流程
     * 配置并行处理任务的逻辑，并启动任务轮询
     */
    private void initializeTaskProcessing() {
        sink.asFlux()
                // 并发数量控制
                .parallel(cpuCores)
                // 确保使用正确的调度器
                .runOn(ioScheduler)
                // 处理每个轨道中的任务
                .doOnNext(task -> {
                    logger.info("开始执行任务: " + task.getTaskId());
                    try {
                        task.run();
                        logger.info("任务执行完毕: " + task.getTaskId());
                    } catch (Exception e) {
                        logger.error("任务执行过程中发生错误: " + e.getMessage(), e);
                    } finally {
                        logger.info("queue size:{}", distributionService.getQueueSize());
                        distributionService.release();
                    }
                })
                // 合并轨道结果
                .sequential()
                // 订阅任务处理逻辑
                .subscribe();
        // 启动任务轮询
        pollForTasks();
    }
    /**
     * 轮询任务
     * 定期从 DistributionService 获取任务，并通过 Sinks 推送到处理流中
     */
    private void pollForTasks() {
        // 动态获取任务
        Mono.defer(() -> Mono.justOrEmpty(distributionService.getNextTask()))
                // 无任务时延迟 1000 毫秒后重试
                .repeatWhenEmpty(repeat -> repeat.delayElements(Duration.ofMillis(1000)))
                // 推送任务到处理流中
                //.doOnNext(sink::tryEmitNext)
                .subscribeOn(ioScheduler)
                // 推送任务到处理流中
                .doOnNext(task -> {
                    // 捕获 tryEmitNext 的返回值
                    Sinks.EmitResult result = sink.tryEmitNext(task);
                    // 根据结果输出日志
                    if (result.isSuccess()) {
                        logger.info("成功推送任务: " + task.getTaskId());
                    } else {
                        logger.error("推送任务失败: " + task.getTaskId() + "，原因: " + result);
                    }
                })
                // 无限次重复获取任务
                .repeat()
                // 在独立线程池中执行
                .subscribe();
    }
}

