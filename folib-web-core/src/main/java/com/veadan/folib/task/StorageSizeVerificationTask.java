package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.email.MailRequest;
import com.veadan.folib.components.email.SendMail;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.ExceedsSizeStorage;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.users.service.FolibUserService;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

/**
 * @author leipenghui
 * 存储告警task
 */
@Slf4j
@Component
//@EnableScheduling
public class StorageSizeVerificationTask {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    @Lazy
    private FolibUserService folibUserService;

    @Inject
    private SendMail sendMail;

    @Value("${folib.temp}")
    private String tempPath;

    @Autowired
    private DistributedLockComponent distributedLockComponent;

    /**
     * 每天8点
     */
    //@Scheduled(cron = "0 0 8 * * ? ")
    public void run() {
        String lockName = "StorageSizeVerificationTask";
        long waitTime = 1L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                storageVerification();
                log.info("StorageSizeVerificationTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 1500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    private void storageVerification() {
        Map<String, Storage> storages = getStorages();
        Map<String, List<ExceedsSizeStorage>> exceedsSizeStorageMap = Maps.newConcurrentMap();
        ExceedsSizeStorage exceedsSizeStorage;
        Storage storage;
        List<ExceedsSizeStorage> exceedsSizeStorageList;
        String storageAdmin;
        for (String storageId : storages.keySet()) {
            storage = storages.get(storageId);
            storageAdmin = storage.getAdmin();
            if (StringUtils.isBlank(storageAdmin)) {
                continue;
            }
            exceedsSizeStorage = storageVerification(storageId, storage);
            if (Objects.nonNull(exceedsSizeStorage)) {
                exceedsSizeStorageList = exceedsSizeStorageMap.get(storageAdmin);
                if (CollectionUtils.isEmpty(exceedsSizeStorageList)) {
                    exceedsSizeStorageList = Lists.newArrayList();
                    exceedsSizeStorageList.add(exceedsSizeStorage);
                    exceedsSizeStorageMap.put(storageAdmin, exceedsSizeStorageList);
                } else {
                    exceedsSizeStorageList.add(exceedsSizeStorage);
                }
            }
        }
        UserDTO userDTO = null;
        for (String admin : exceedsSizeStorageMap.keySet()) {
            userDTO = folibUserService.findByUserName(admin);
            if (Objects.isNull(userDTO)) {
                continue;
            }
            if (StringUtils.isBlank(userDTO.getEmail())) {
                continue;
            }
            exceedsSizeStorageList = exceedsSizeStorageMap.get(admin);
            //发送邮件
            handlerDataAndSendEmail(userDTO.getEmail(), exceedsSizeStorageList);
        }
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private ExceedsSizeStorage storageVerification(String storageId, Storage storage) {
        ExceedsSizeStorage exceedsSizeStorage = null;
        Long storageMaxSize = storage.getStorageMaxSize();
        if (Objects.isNull(storageMaxSize) || storageMaxSize <= 0) {
            return null;
        }
        long storageBytesSize = artifactRepository.artifactsBytesStatisticsByStorageIds(Collections.singletonList(storageId));
        BigDecimal storageMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageMaxSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal storageRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageBytesSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal useStorageProportion = storageRealTbSize.divide(storageMaxTbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        //占比大于95的
        if (useStorageProportion.compareTo(BigDecimal.valueOf(95)) >= 0) {
            log.warn("The size of the storage [{}] exceeds the maximum size accepted by " +
                    "this repository ({}}/{}}) unit {}.", storageId, storageRealTbSize, storageMaxTbSize, FileUnitTypeEnum.TB.getUnit());
            exceedsSizeStorage = ExceedsSizeStorage.builder().storageId(storageId).storageSize(storageMaxTbSize).useStorageSize(storageRealTbSize).useStorageProportion(useStorageProportion).build();
        }
        return exceedsSizeStorage;
    }

    /**
     * 处理数据、发送邮件
     *
     * @param email                  接收邮箱
     * @param exceedsSizeStorageList 数据
     */
    private void handlerDataAndSendEmail(String email, List<ExceedsSizeStorage> exceedsSizeStorageList) {
        try {
            if (StringUtils.isBlank(email)) {
                return;
            }
            String filePath = tempPath + File.separator + UUID.randomUUID() + ".xlsx";
            File file = FileUtil.file(filePath);
            FileUtil.mkdir(file.getParent());
            FileOutputStream fileOutputStream = new FileOutputStream(file);
            try {
                InputStream template = this.getClass().getResourceAsStream("/template/exceedSizeStorageTemplate.xlsx");
                try (ExcelWriter excelWriter = EasyExcel.write(fileOutputStream).withTemplate(template).build()) {
                    WriteSheet writeSheet = EasyExcel.writerSheet().build();
                    FillConfig fillConfig = FillConfig.builder().build();
                    if (CollectionUtils.isNotEmpty(exceedsSizeStorageList)) {
                        List<List<ExceedsSizeStorage>> list = Lists.partition(exceedsSizeStorageList, 10);
                        for (List<ExceedsSizeStorage> itemList : list) {
                            // 放入数据
                            excelWriter.fill(itemList, fillConfig, writeSheet);
                        }
                    } else {
                        excelWriter.fill(Collections.emptyList(), fillConfig, writeSheet);
                    }
                    excelWriter.finish();
                    sendMail.sendHtmlMail(MailRequest.builder().filePath(filePath).sendTo(email).subject("存储空间存储额度告警通知").text("此邮件为存储空间存储额度告警通知邮件，详情见附件").build());
                }
            } finally {
                fileOutputStream.close();
                FileUtil.del(file);
            }
        } catch (Exception ex) {
            log.error("发送存储空间存储额度告警通知邮件错误：{}", ExceptionUtils.getStackTrace(ex));
        }
    }
}
