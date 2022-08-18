package com.veadan.folib.scanner.common.util;//
//
//package com.veadan.folib.scanner.common.util;
//
//
//import com.veadan.folib.store.biz.DownloadRecordBiz;
//import com.veadan.folib.store.entity.DownloadRecord;
//import lombok.extern.slf4j.Slf4j;
//
//import java.util.ArrayList;
//import java.util.List;
//import java.util.concurrent.BlockingQueue;
//import java.util.concurrent.LinkedBlockingQueue;
//
///**
// * ${DESCRIPTION}
// *
// * @author Veadan
// * @version 2018-07-01 15:28
// */
//@Slf4j
//public class FolibLog extends Thread {
//    private static FolibLog dblog = null;
//    private static BlockingQueue<DownloadRecord> logInfoQueue = new LinkedBlockingQueue<DownloadRecord>(1024);
//
//    public DownloadRecordBiz getLogService() {
//        return logService;
//    }
//
//    public FolibLog setLogService(DownloadRecordBiz logService) {
//        if(this.logService==null) {
//            this.logService = logService;
//        }
//        return this;
//    }
//
//    private DownloadRecordBiz logService;
//    public static synchronized FolibLog getInstance() {
//        if (dblog == null) {
//            dblog = new FolibLog();
//        }
//        return dblog;
//    }
//
//    private FolibLog() {
//        super("CLogOracleWriterThread");
//    }
//
//    public void offerQueue(DownloadRecord logInfo) {
//        try {
//            logInfoQueue.offer(logInfo);
//        } catch (Exception e) {
//            log.error("日志写入失败", e);
//        }
//    }
//
//    @Override
//    public void run() {
//        List<DownloadRecord> bufferedLogList = new ArrayList<DownloadRecord>(); // 缓冲队列
//        while (true) {
//            try {
//                bufferedLogList.add(logInfoQueue.take());
//                logInfoQueue.drainTo(bufferedLogList);
//                if (bufferedLogList != null && bufferedLogList.size() > 0) {
//                    // 写入日志
//                    for(DownloadRecord log:bufferedLogList){
//                        logService.insertSelective(log);
//                    }
//                }
//            } catch (Exception e) {
//                e.printStackTrace();
//                // 防止缓冲队列填充数据出现异常时不断刷屏
//                try {
//                    Thread.sleep(1000);
//                } catch (Exception eee) {
//                }
//            } finally {
//                if (bufferedLogList != null && bufferedLogList.size() > 0) {
//                    try {
//                        bufferedLogList.clear();
//                    } catch (Exception e) {
//                    }
//                }
//            }
//        }
//    }
//}
