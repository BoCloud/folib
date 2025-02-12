package com.veadan.folib.task;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.alibaba.fastjson.JSONObject;
import com.github.pagehelper.PageInfo;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.email.MailRequest;
import com.veadan.folib.components.email.SendMail;
import com.veadan.folib.configuration.AlarmConfiguration;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.CapacityStorage;
import com.veadan.folib.domain.ExceedsSizeStorage;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.StorageMonitoringService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.FolibUserService;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Slf4j
@Component
public class AlarmNoticeTask {

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

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private StorageMonitoringService storageMonitoringService;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;


    @Async("asyncThreadPoolTaskExecutor")
    public void immediateExecutionNotice() {
        this.someMethod();
    }

    public void someMethod() {
        String lockName = "AlarmNoticeTask";
        long waitTime = 1L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                storageVerification();
                log.info("AlarmNoticeTask thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 1500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    private void storageVerification() {
        log.info("存储告警:开始处理");
        //获取所有存储空间
        Map<String, Storage> storages = getStorages();
        if(Objects.isNull(storages) || storages.isEmpty()){
            log.warn("storages is null or empty");
        }
        Configuration configuration = configurationManagementService.getConfiguration();
        AlarmConfiguration alarmConfiguration = configuration.getAlarmConfiguration();
        boolean isAdmin = false;
        boolean isStorageAdmin = false;
        Set<String> emaiList = new HashSet<>();
        if (alarmConfiguration.getNotificationPolicy() != null && !alarmConfiguration.getNotificationPolicy().isEmpty()) {
            isAdmin = alarmConfiguration.getNotificationPolicy().stream().anyMatch(policy -> policy.equals("admin"));
            isStorageAdmin = alarmConfiguration.getNotificationPolicy().stream().anyMatch(policy -> policy.equals("storageAdmin"));
        }

        List<String> userList = Lists.newArrayList();

        if (alarmConfiguration.getRecipients() != null && !alarmConfiguration.getRecipients().isEmpty()) {
            userList.addAll(alarmConfiguration.getRecipients());
        }
        if (alarmConfiguration.getEmails() != null && !alarmConfiguration.getEmails().isEmpty()) {
            emaiList.addAll(alarmConfiguration.getEmails());
        }

        List<CapacityStorage> stroageList = Lists.newArrayList();
        for (String storageId : storages.keySet()) {
            StorageData storeData = (StorageData) storages.get(storageId);
            if (storeData.getStorageMaxSize() > 0) {
                CapacityStorage capacityStorage = new CapacityStorage();
                capacityStorage.setStorageId(storageId);
                capacityStorage.setStorageSize(BigDecimal.valueOf(storeData.getStorageMaxSize()));
                long storageBytesSize = artifactRepository.artifactsBytesStatisticsByStorageIds(Collections.singletonList(storageId));
                capacityStorage.setUseStorageSize(BigDecimal.valueOf(storageBytesSize));
                capacityStorage.setPlatformStorageThreshold(alarmConfiguration.getStorageThreshold());
                storageVerification(capacityStorage);
                stroageList.add(capacityStorage);
                log.info("存储告警:存储空间[{}]设置阈值:[{}],当前使用:[{}]", storeData.getId(),storeData.getStorageMaxSize(),storageBytesSize);
            }else {
                log.warn("存储告警:存储空间[{}]未设置阈值",storeData.getId());
            }

            if (isStorageAdmin) {
                userList.add(storeData.getAdmin());
            }
            if(storeData.getRepositories().keySet().isEmpty()){
                log.warn("{} repository is null or empty",storageId);
            }
            for (String repositoryId : storeData.getRepositories().keySet()) {
                RepositoryData repositoryDto = (RepositoryData) storeData.getRepository(repositoryId);
                if (repositoryDto.getStorageMaxSize() > 0) {
                    CapacityStorage repoStorage = new CapacityStorage();
                    repoStorage.setRepositoryId(repositoryId);
                    repoStorage.setStorageId(storeData.getId());
                    repoStorage.setStorageSize(BigDecimal.valueOf(repositoryDto.getStorageMaxSize()));
                    long storageBytesSize = artifactRepository.artifactsBytesStatistics(Collections.singletonList(String.format("%s-%s", storageId, repositoryId)));
                    repoStorage.setUseStorageSize(BigDecimal.valueOf(storageBytesSize));
                    repoStorage.setPlatformStorageThreshold(alarmConfiguration.getStorageThreshold());
                    repoStorage.setStorageThreshold(repositoryDto.getStorageThreshold());
                    repositoriesVerification(repoStorage);
                    stroageList.add(repoStorage);
                    log.info("存储告警:存储空间[{}]仓库[{}]设置阈值[{}],当前使用:[{}]", repositoryId, storeData.getId(),repositoryDto.getStorageMaxSize(),storageBytesSize);
                }else {
                    log.warn("存储告警:存储空间[{}]仓库[{}]未设置阈值", repositoryId, storeData.getId());
                }
            }
        }

        PageInfo<UserDTO> pageInfo = folibUserService.getUsers(new UserDto(), 1, 10000);
        List<UserDTO> users = pageInfo.getList();

        if (isAdmin) {
            emaiList.addAll(users.stream().filter(user -> user.getRoles().contains("ADMIN")).map(UserDTO::getEmail).collect(Collectors.toList()));
            log.info("存储告警:需要通知的管理员[{}]", JSONObject.toJSON(emaiList));
        }
        if (isStorageAdmin) {
            emaiList.addAll(users.stream().filter(user -> userList.contains(user.getUsername())).map(UserDTO::getEmail).collect(Collectors.toList()));
            log.info("存储告警:需要通知的存储空间管理员[{}]", JSONObject.toJSON(emaiList));
        }
        if (emaiList.isEmpty()) {
            log.warn("存储告警:没有需要通知的用户的邮箱");
            return;
        }

        stroageList = stroageList.stream().filter(CapacityStorage::isNotice).collect(Collectors.toList());
        log.info("存储告警:需要通知的存储空间[{}]", JSONObject.toJSON(stroageList.stream()));
        if (!stroageList.isEmpty()) {
            for (String email : emaiList) {
                //发送邮件
                handlerDataAndSendEmail(email, stroageList);
            }
        }else {
            log.warn("存储告警:没有需要通知的存储空间");
        }
        log.info("存储告警:处理完成");
    }


    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    /**
     * 验证存储容量
     *
     * @param capacityStorage 存储容量对象
     */
    public void storageVerification(CapacityStorage capacityStorage) {
        if (Objects.isNull(capacityStorage.getStorageSize()) || capacityStorage.getStorageSize().compareTo(BigDecimal.ZERO) <= 0) {
            capacityStorage.setNotice(false);
        }
        double threshold = 0.9;
        if (capacityStorage.getRepositoryId() != null && capacityStorage.getStorageThreshold() > 0) {
            threshold = capacityStorage.getStorageThreshold();
        } else if (capacityStorage.getPlatformStorageThreshold() > 0) {
            threshold = capacityStorage.getPlatformStorageThreshold();
        }

        // 将最大存储尺寸从字节转换为太字节（TB）
        BigDecimal storageMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(capacityStorage.getStorageSize().longValue(), FileUnitTypeEnum.GB.getUnit());
        // 将实际存储尺寸从字节转换为太字节（TB）
        BigDecimal storageRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(capacityStorage.getUseStorageSize().longValue(), FileUnitTypeEnum.GB.getUnit());
        // 计算存储使用比例
        BigDecimal useStorageProportion = storageRealTbSize.divide(storageMaxTbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        // 如果存储使用比例大于或等于95%，则记录警告日志并创建ExceedsSizeStorage对象
        if (useStorageProportion.compareTo(BigDecimal.valueOf(threshold * 100)) >= 0) {
            log.warn("The size of the storage [{}] exceeds the maximum size accepted by " +
                    "this repository ({}/{}/{}) unit {}.", capacityStorage.getStorageId(), capacityStorage.getRepositoryId(), storageRealTbSize, storageMaxTbSize, FileUnitTypeEnum.TB.getUnit());
            capacityStorage.setNotice(true);
        }
        capacityStorage.setStorageSize(storageMaxTbSize);
        capacityStorage.setUseStorageProportion(useStorageProportion);
        capacityStorage.setUseStorageSize(storageRealTbSize);

    }

    public void repositoriesVerification(CapacityStorage capacityStorage) {
        if (Objects.isNull(capacityStorage.getStorageSize()) || capacityStorage.getStorageSize().compareTo(BigDecimal.ZERO) <= 0) {
            capacityStorage.setNotice(false);
        }
        double threshold = 0.9;
        if (capacityStorage.getRepositoryId() != null && capacityStorage.getStorageThreshold() > 0) {
            threshold = capacityStorage.getStorageThreshold();
        } else if (capacityStorage.getPlatformStorageThreshold() > 0) {
            threshold = capacityStorage.getPlatformStorageThreshold();
        }
        // 将最大存储尺寸从字节转换为太字节（TB）
        BigDecimal storageMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(capacityStorage.getStorageSize().longValue(), FileUnitTypeEnum.GB.getUnit());
        // 将实际存储尺寸从字节转换为太字节（TB）
        BigDecimal storageRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(capacityStorage.getUseStorageSize().longValue(), FileUnitTypeEnum.GB.getUnit());
        // 计算存储使用比例
        BigDecimal useStorageProportion = storageRealTbSize.divide(storageMaxTbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        // 如果存储使用比例大于或等于95%，则记录警告日志并创建ExceedsSizeStorage对象
        if (useStorageProportion.compareTo(BigDecimal.valueOf(threshold * 100)) >= 0) {
            log.warn("The size of the storage [{}] exceeds the maximum size accepted by " +
                    "this repository ({}/{}/{}) unit {}.", capacityStorage.getStorageId(), capacityStorage.getRepositoryId(), storageRealTbSize, storageMaxTbSize, FileUnitTypeEnum.TB.getUnit());
            capacityStorage.setNotice(true);
        }
        capacityStorage.setStorageSize(storageMaxTbSize);
        capacityStorage.setUseStorageProportion(useStorageProportion);
        capacityStorage.setUseStorageSize(storageRealTbSize);

    }


    /**
     * 处理数据、发送邮件
     *
     * @param email                  接收邮箱
     * @param exceedsSizeStorageList 数据
     */
    private void handlerDataAndSendEmail(String email, List<CapacityStorage> exceedsSizeStorageList) {

        // 检查邮箱地址是否为空，为空则直接返回
        if (StringUtils.isBlank(email)) {
            log.warn("存储告警:没有需要通知的用户的邮箱");
            return;
        }
        log.info("存储告警:开始发送邮件[{}]", email);
        // 生成临时文件路径，用于保存Excel文件
        String filePath = tempPath + File.separator + UUID.randomUUID() + ".xlsx";
        File file = FileUtil.file(filePath);
        // 确保父目录存在
        FileUtil.mkdir(file.getParent());
        // 创建文件输出流
        try (FileOutputStream fileOutputStream = new FileOutputStream(file);
             InputStream template = this.getClass().getResourceAsStream("/template/exceedSizeStorageTemplate.xlsx");
             ExcelWriter excelWriter = EasyExcel.write(fileOutputStream).withTemplate(template).build();) {

            // 创建写入工作表的对象
            WriteSheet writeSheet = EasyExcel.writerSheet().build();
            // 创建填充配置对象
            FillConfig fillConfig = FillConfig.builder().build();
            // 检查是否有数据需要写入
            if (CollectionUtils.isNotEmpty(exceedsSizeStorageList)) {
                // 如果数据量大，分批处理以避免内存溢出
                List<List<CapacityStorage>> list = Lists.partition(exceedsSizeStorageList, 10);
                for (List<CapacityStorage> itemList : list) {
                    // 放入数据
                    excelWriter.fill(itemList, fillConfig, writeSheet);
                }
            } else {
                // 如果没有数据，也需告知
                excelWriter.fill(Collections.emptyList(), fillConfig, writeSheet);
            }
            // 完成写入
            excelWriter.finish();
            // 构建邮件请求并发送邮件
            sendMail.sendHtmlMail(MailRequest.builder().filePath(filePath).sendTo(email).subject("存储空间存储额度告警通知").text("此邮件为存储空间存储额度告警通知邮件，详情见附件").build());
        } catch (Exception ex) {
            log.error("发送存储空间存储额度告警通知邮件错误：{}", ExceptionUtils.getStackTrace(ex));
        } finally {
            log.info("存储告警:结束发送邮件[{}]", email);
            // 删除临时文件
            FileUtil.del(file);
        }
    }
}
