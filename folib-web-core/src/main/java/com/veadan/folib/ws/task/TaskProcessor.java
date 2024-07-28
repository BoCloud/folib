package com.veadan.folib.ws.task;

;
import com.veadan.folib.ws.server.DistributionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

    /**
     * TaskProcessor 构造函数
     * 初始化分发服务、任务执行器和任务触发器
     *
     * @param distributionService 注入的 DistributionService 实例
     */
    @Autowired
    public TaskProcessor(DistributionService distributionService) throws NoSuchFieldException, IllegalAccessException {
        this.distributionService = distributionService;
        // this.taskExecutor = taskExecutor;
        this.sink = Sinks.many().multicast().onBackpressureBuffer();
        //ThreadPoolExecutor executor = new ThreadPoolExecutor(
        //        10, // 核心线程数
        //        50, // 最大线程数
        //        60, // 线程空闲超时
        //        TimeUnit.SECONDS, // 超时单位
        //        new java.util.concurrent.LinkedBlockingQueue<>(100) // 队列容量
        //);

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
        // 将 Sinks.Many 转换为 Flux，并配置并行处理和错误处理
        sink.asFlux()
                .parallel(8)
                // 确保使用正确的调度器
                .runOn(ioScheduler)
                .subscribe(task -> {
                            logger.info("====================================================================================================");
                            logger.info("开始执行任务: " + task.getTaskId());
                            task.run();
                            logger.info("任务执行完毕: " + task.getTaskId());
                            logger.info("queue size:{}", distributionService.getQueueSize());
                            logger.info("====================================================================================================");
                            },
                        throwable -> {
                            throwable.printStackTrace();
                        });
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
                .doOnNext(task -> sink.tryEmitNext(task))
                // 无限次重复获取任务
                .repeat()
                .subscribeOn(ioScheduler) // 在独立线程池中执行
                .subscribe();
    }
}

