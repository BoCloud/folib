package com.veadan.folib.cron.jobs;

import cn.hutool.core.io.FileUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.email.MailRequest;
import com.veadan.folib.components.email.SendMail;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.domain.ExceedsSizeRepository;
import com.veadan.folib.domain.User;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repositories.UserRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.FileSizeConvertUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

/**
 * @author veadan
 */
public class RepositorySizeVerificationCronJob
        extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private UserRepository userRepository;

    @Inject
    private SendMail sendMail;

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                return;
            }
            ExceedsSizeRepository exceedsSizeRepository = repositoryVerification(storageId, repositoryId, repository);
            if (Objects.isNull(exceedsSizeRepository)) {
                return;
            }
            String admin = repository.getStorage().getAdmin();
            if (StringUtils.isBlank(admin)) {
                return;
            }
            Optional<User> userOptional = userRepository.findById(admin);
            if (userOptional.isEmpty()) {
                return;
            }
            User user = userOptional.get();
            if (StringUtils.isBlank(user.getEmail())) {
                return;
            }
            handlerDataAndSendEmail(user.getEmail(), Lists.newArrayList(exceedsSizeRepository));
        } else {
            storaegsVerification();
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RepositorySizeVerificationCronJob.class.getName())
                .name("校验仓库存储配额使用情况")
                .scope(GLOBAL)
                .description("该任务用于校验仓库存储配额使用情况")
                .fields(FIELDS)
                .build();
    }

    /**
     * To size verification in repositories
     *
     * @throws IOException
     */
    private void storaegsVerification()
            throws IOException {
        Map<String, Storage> storages = getStorages();
        Map<String, List<ExceedsSizeRepository>> exceedSizeRepositories = Maps.newConcurrentMap();
        ExceedsSizeRepository exceedsSizeRepository;
        Storage storage;
        Repository repository;
        List<ExceedsSizeRepository> exceedsSizeRepositoryList;
        String storageAdmin;
        for (String storageId : storages.keySet()) {
            storage = storages.get(storageId);
            storageAdmin = storage.getAdmin();
            if (StringUtils.isBlank(storageAdmin)) {
                continue;
            }
            Map<String, ? extends Repository> repositories = getRepositories(storageId);
            for (String repositoryId : repositories.keySet()) {
                repository = repositories.get(repositoryId);
                if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    continue;
                }
                exceedsSizeRepository = repositoryVerification(storageId, repositoryId, repository);
                if (Objects.nonNull(exceedsSizeRepository)) {
                    exceedsSizeRepositoryList = exceedSizeRepositories.get(storageAdmin);
                    if (CollectionUtils.isEmpty(exceedsSizeRepositoryList)) {
                        exceedsSizeRepositoryList = Lists.newArrayList();
                        exceedsSizeRepositoryList.add(exceedsSizeRepository);
                        exceedSizeRepositories.put(storageAdmin, exceedsSizeRepositoryList);
                    } else {
                        exceedsSizeRepositoryList.add(exceedsSizeRepository);
                    }
                }
            }
        }
        Optional<User> userOptional;
        User user;
        for (String admin : exceedSizeRepositories.keySet()) {
            userOptional = userRepository.findById(admin);
            if (userOptional.isEmpty()) {
                continue;
            }
            user = userOptional.get();
            if (StringUtils.isBlank(user.getEmail())) {
                continue;
            }
            exceedsSizeRepositoryList = exceedSizeRepositories.get(admin);
            //发送邮件
            handlerDataAndSendEmail(user.getEmail(), exceedsSizeRepositoryList);
        }
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId) {
        return getStorages().get(storageId).getRepositories();
    }

    private ExceedsSizeRepository repositoryVerification(String storageId, String repositoryId, Repository repository) {
        ExceedsSizeRepository exceedsSizeRepository = null;
        long repositoryMaxSize = repository.getRepositoryMaxSize();
        if (repositoryMaxSize <= 0) {
            return null;
        }
        long repositoryBytesSize = artifactRepository.artifactsBytesStatistics(Collections.singletonList(String.format("%s-%s", storageId, repositoryId)));
        BigDecimal repositoryMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(repositoryMaxSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal repositoryRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(repositoryBytesSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal useRepositoryProportion = repositoryRealTbSize.divide(repositoryMaxTbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        //占比大于95的
        if (useRepositoryProportion.compareTo(BigDecimal.valueOf(95)) >= 0) {
            logger.warn("The size of the storage [{}] repository [{}] exceeds the maximum size accepted by " +
                    "this repository ({}}/{}}) unit {}.", storageId, repositoryId, repositoryRealTbSize, repositoryMaxTbSize, FileUnitTypeEnum.TB.getUnit());
            exceedsSizeRepository = ExceedsSizeRepository.builder().storageId(storageId).repositoryId(repositoryId).layout(repository.getLayout()).repositoryMaxSize(repositoryMaxTbSize).useRepositorySize(repositoryRealTbSize).useRepositoryProportion(useRepositoryProportion).build();
        }
        return exceedsSizeRepository;
    }

    /**
     * 处理数据、发送邮件
     *
     * @param email                     接收邮箱
     * @param exceedsSizeRepositoryList 数据
     */
    private void handlerDataAndSendEmail(String email, List<ExceedsSizeRepository> exceedsSizeRepositoryList) {
        try {
            if (StringUtils.isBlank(email)) {
                return;
            }
            String filePath = tempPath + File.separator + UUID.randomUUID() + ".xlsx";
            File file = FileUtil.file(filePath);
            FileUtil.mkdir(file.getParent());
            FileOutputStream fileOutputStream = new FileOutputStream(file);
            try {
                InputStream template = this.getClass().getResourceAsStream("/template/exceedSizeRepositoryTemplate.xlsx");
                try (ExcelWriter excelWriter = EasyExcel.write(fileOutputStream).withTemplate(template).build()) {
                    WriteSheet writeSheet = EasyExcel.writerSheet().build();
                    FillConfig fillConfig = FillConfig.builder().build();
                    if (CollectionUtils.isNotEmpty(exceedsSizeRepositoryList)) {
                        List<List<ExceedsSizeRepository>> list = Lists.partition(exceedsSizeRepositoryList, 50);
                        for (List<ExceedsSizeRepository> itemList : list) {
                            // 放入数据
                            excelWriter.fill(itemList, fillConfig, writeSheet);
                        }
                    } else {
                        excelWriter.fill(Collections.emptyList(), fillConfig, writeSheet);
                    }
                    excelWriter.finish();
                    sendMail.sendHtmlMail(MailRequest.builder().filePath(filePath).sendTo(email).subject("仓库存储额度告警通知").text("此邮件为仓库存储额度告警通知邮件，详情见附件").build());
                }
            } finally {
                fileOutputStream.close();
                FileUtil.del(file);
            }
        } catch (Exception ex) {
            logger.error("发送漏洞邮件错误：{}", ExceptionUtils.getStackTrace(ex));
        }
    }

}
