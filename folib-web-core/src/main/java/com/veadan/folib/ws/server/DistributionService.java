package com.veadan.folib.ws.server;

import cn.hutool.core.collection.CollectionUtil;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.util.CollectionUtils;
import com.veadan.folib.ws.task.DistributionTask;
import com.veadan.folib.ws.task.OptimizedDynamicPriorityBlockingQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Semaphore;

@Service
public class DistributionService {


    private static final Logger log = LoggerFactory.getLogger(DistributionService.class);

    @Autowired
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;

    private static final int MAX_SIZE = Runtime.getRuntime().availableProcessors();

    public static final Semaphore semaphore = new Semaphore(4);

    /**
     * 创建优先级队列，使用自然排序
     */
    private final OptimizedDynamicPriorityBlockingQueue<DistributionTask> queue = new OptimizedDynamicPriorityBlockingQueue<>(Comparator.naturalOrder());

    /**
     * 添加任务
     *
     * @param task
     */
    public void addTask(DistributionTask task) {
        log.info("addTask: " + task.getTaskId() + " priority: " + task.getPriority());
        log.info("当前的可用许可数量: " + semaphore.availablePermits());
        queue.put(task, task.getTaskId());
        log.info("addTask queue size:{}: " ,queue.size());
    }

    /**
     * 更新任务优先级
     *
     * @param taskId  任务ID
     * @param newPriority 新优先级
     */
    public void updateTaskPriority(String taskId, int newPriority) {
        DistributionTask taskToAdjust = queue.getElementById(taskId);
        if (taskToAdjust != null) {
            taskToAdjust.setPriority(newPriority);
            log.info("updateTaskPriority: " + taskToAdjust.getTaskId() + " priority: " + taskToAdjust.getPriority());
            queue.adjustPriority(taskToAdjust, Comparator.comparingInt(DistributionTask::getPriority).reversed());
        }
        throw new RuntimeException("Task not found taskId:"+taskId);
    }

    /**
     * 获取下一个任务
     *
     * @return DistributionTask
     */
    public DistributionTask getNextTask() {
        DistributionTask task = null;
        try {
            log.info("getNextTask: " + queue.size());
            // 获取信号量
            acquire();
            task = queue.take();
            if(task!=null){
                log.info("getNextTask queue size:{} ",queue.size());
            }
        } catch (InterruptedException e) {
            log.error("InterruptedException: " + e.getMessage());
        }
        return task;
    }

    /**
     * 获取队列大小
     *
     * @return int
     */
    public int getQueueSize() {
        return queue.size();
    }

    /**
     * 根据节点名称清空任务队列
     *
     * @param targetHostName
     */
    public void clearByNodeTaskQueue(String targetHostName) {
       List<String> syncNoList = artifactSyncRecordMapper.searchByTargetHostName(targetHostName);
       if(CollectionUtil.isNotEmpty(syncNoList)){
         syncNoList.stream().forEach(syncNo -> {
             DistributionTask task = queue.getElementById(syncNo);
             if(task!=null){
                 queue.remove(task);
             }
         });
       }
    }

    /**
     * 获取信号量
     *
     * @throws InterruptedException
     */
    public void acquire() throws InterruptedException {
        semaphore.acquire();
        log.info("当前的可用许可数量: " + semaphore.availablePermits());
    }

    /**
     * 释放信号量
     */
    public void release() {
        semaphore.release();
        log.info("当前的可用许可数量: " + semaphore.availablePermits());
    }
}
