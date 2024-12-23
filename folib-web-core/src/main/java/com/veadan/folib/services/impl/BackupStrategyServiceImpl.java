package com.veadan.folib.services.impl;

import com.datastax.oss.driver.shaded.guava.common.collect.Maps;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.components.backup.BackupComponent;
import com.veadan.folib.components.cron.CronComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.cron.jobs.backups.ArtifactBackupCronJob;
import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.entity.BackupStrategy;
import com.veadan.folib.entity.BackupStrategyRepository;
import com.veadan.folib.entity.License;
import com.veadan.folib.forms.backupstrategy.BackupStrategyForm;
import com.veadan.folib.mapper.BackupStrategyMapper;
import com.veadan.folib.mapper.BackupStrategyRepositoryMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.BackupStrategyService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/12/17
 **/
@Slf4j
@Service
public class BackupStrategyServiceImpl implements BackupStrategyService {

    private final static String REPOSITORY_BACKUP_STRATEGY_KEY = "REPOSITORY_BACKUP_STRATEGY_KEY";

    @Autowired
    private BackupStrategyMapper backupStrategyMapper;

    @Autowired
    private BackupStrategyRepositoryMapper backupStrategyRepositoryMapper;

    @Autowired
    private ConfigurationManager configurationManager;

    @Autowired
    private IdGenerateUtils idGenerateUtils;

    @Autowired
    private HazelcastInstance hazelcastInstance;

    @Autowired
    private CronComponent cronComponent;

    @Autowired
    private BackupComponent backupComponent;

    @Override
    public TableResultResponse<BackupStrategyRecord> queryBackupStrategyPage(Integer page, Integer limit, BackupStrategyForm backupStrategyForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<BackupStrategyRecord> backupStrategyList = backupStrategyMapper.selectList(backupStrategyForm);
        if (CollectionUtils.isEmpty(backupStrategyList)) {
            backupStrategyList = Collections.emptyList();
        }
        return new TableResultResponse<BackupStrategyRecord>(result.getTotal(), backupStrategyList);
    }

    @Override
    public List<BackupStrategyRecord> queryBackupStrategyList(BackupStrategyForm backupStrategyForm) {
        List<BackupStrategyRecord> backupStrategyList = backupStrategyMapper.selectInfoList(backupStrategyForm);
        return backupStrategyList;
    }

    @Override
    public BackupStrategyForm queryBackupStrategy(BackupStrategy backupStrategy) {
        BackupStrategyForm backupStrategyForm = null;
        String backupStrategyName = backupStrategy.getStrategyName();
        BackupStrategy existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        if (Objects.isNull(existsBackupStrategy)) {
            return null;
        }
        backupStrategyForm = BackupStrategyForm.builder().build();
        BeanUtils.copyProperties(existsBackupStrategy, backupStrategyForm);
        backupStrategyForm.setId(existsBackupStrategy.getId().toString());

        //查询仓库
        backupStrategyForm.setRepositories(getRepositories(existsBackupStrategy.getId()));
        return backupStrategyForm;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveBackupStrategy(BackupStrategyForm backupStrategyForm) {
        String backupStrategyName = backupStrategyForm.getStrategyName();
        BackupStrategy existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        if (Objects.nonNull(existsBackupStrategy)) {
            return;
        }
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long backupStrategyId = idGenerateUtils.generateId("backupStrategyId");
        BackupStrategy backupStrategy = BackupStrategy.builder().strategyName(backupStrategyName).createBy(username).createTime(now)
                .enabled(true).id(backupStrategyId).backupPath(backupStrategyForm.getBackupPath()).incremental(backupStrategyForm.getIncremental())
                .retentionPeriod(backupStrategyForm.getRetentionPeriod()).cronExpression(backupStrategyForm.getCronExpression()).updateBy(username).updateTime(now).build();
        if (Boolean.FALSE.equals(backupStrategyForm.getEnabled())) {
            backupStrategy.setEnabled(false);
        }
        backupStrategyMapper.insertSelective(backupStrategy);
        if (CollectionUtils.isNotEmpty(backupStrategyForm.getRepositories())) {
            String storageId = "", repositoryId = "";
            Storage storage = null;
            Repository repository = null;
            List<BackupStrategyRepository> backupStrategyRepositoryList = Lists.newArrayList();
            BackupStrategyRepository backupStrategyRepository = null;
            for (String storageIdAndRepositoryId : backupStrategyForm.getRepositories()) {
                storageId = ConfigurationUtils.getStorageId("", storageIdAndRepositoryId);
                repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
                storage = configurationManager.getStorage(storageId);
                if (Objects.isNull(storage)) {
                    continue;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    continue;
                }
                backupStrategyRepository = BackupStrategyRepository.builder().id(idGenerateUtils.generateId("backupStrategyRepositoryId"))
                        .backupStrategyId(backupStrategyId).createBy(username).createTime(now).storageId(storageId).repositoryId(repositoryId).updateBy(username).updateTime(now).build();
                backupStrategyRepositoryList.add(backupStrategyRepository);
                clearCache(storageId, repositoryId);
            }
            if (CollectionUtils.isNotEmpty(backupStrategyRepositoryList)) {
                List<List<BackupStrategyRepository>> backupStrategyRepositories = Lists.partition(backupStrategyRepositoryList, 100);
                for (List<BackupStrategyRepository> itemList : backupStrategyRepositories) {
                    backupStrategyRepositoryMapper.batchInsertBackupStrategyRepository(itemList);
                }
            }
        }
        createCronTask(backupStrategy.getStrategyName(), backupStrategy.getEnabled(), backupStrategy.getStrategyName(), backupStrategy.getCronExpression(), "", backupStrategy.getBackupPath(), backupStrategy.getIncremental(), backupStrategyForm.getRepositories());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateBackupStrategy(BackupStrategyForm backupStrategyForm) {
        String backupStrategyName = backupStrategyForm.getStrategyName();
        BackupStrategy existsBackupStrategy = null;
        if (StringUtils.isNotBlank(backupStrategyForm.getId())) {
            existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().id(Long.parseLong(backupStrategyForm.getId())).build());
        } else {
            existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        }
        if (Objects.isNull(existsBackupStrategy)) {
            return;
        }
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long backupStrategyId = existsBackupStrategy.getId();
        BackupStrategy backupStrategy = BackupStrategy.builder().strategyName(existsBackupStrategy.getStrategyName()).createBy(username).createTime(now)
                .enabled(true).id(backupStrategyId).backupPath(backupStrategyForm.getBackupPath()).incremental(backupStrategyForm.getIncremental())
                .retentionPeriod(backupStrategyForm.getRetentionPeriod()).cronExpression(backupStrategyForm.getCronExpression()).updateBy(username).updateTime(now).build();
        if (Boolean.FALSE.equals(backupStrategyForm.getEnabled())) {
            backupStrategy.setEnabled(false);
        }
        backupStrategyMapper.updateByPrimaryKey(backupStrategy);
        Example example = new Example(BackupStrategyRepository.class);
        example.createCriteria().andEqualTo("backupStrategyId", backupStrategyId);
        backupStrategyRepositoryMapper.deleteByExample(example);
        clearBackupStrategyRepositoryCache(existsBackupStrategy.getId());
        if (CollectionUtils.isNotEmpty(backupStrategyForm.getRepositories())) {
            String storageId = "", repositoryId = "";
            Storage storage = null;
            Repository repository = null;
            List<BackupStrategyRepository> backupStrategyRepositoryList = Lists.newArrayList();
            BackupStrategyRepository backupStrategyRepository = null;
            for (String storageIdAndRepositoryId : backupStrategyForm.getRepositories()) {
                storageId = ConfigurationUtils.getStorageId("", storageIdAndRepositoryId);
                repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
                storage = configurationManager.getStorage(storageId);
                if (Objects.isNull(storage)) {
                    continue;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    continue;
                }
                backupStrategyRepository = BackupStrategyRepository.builder().id(idGenerateUtils.generateId("backupStrategyRepositoryId"))
                        .backupStrategyId(backupStrategyId).createBy(username).createTime(now).storageId(storageId).repositoryId(repositoryId).build();
                backupStrategyRepositoryList.add(backupStrategyRepository);
                //清理当前传入的仓库缓存
                clearCache(storageId, repositoryId);
            }
            if (CollectionUtils.isNotEmpty(backupStrategyRepositoryList)) {
                List<List<BackupStrategyRepository>> backupStrategyRepositories = Lists.partition(backupStrategyRepositoryList, 100);
                for (List<BackupStrategyRepository> itemList : backupStrategyRepositories) {
                    backupStrategyRepositoryMapper.batchInsertBackupStrategyRepository(itemList);
                }
            }
        }
        createCronTask(backupStrategy.getStrategyName(), backupStrategy.getEnabled(), backupStrategy.getStrategyName(), backupStrategy.getCronExpression(), existsBackupStrategy.getCronExpression(), backupStrategy.getBackupPath(), backupStrategy.getIncremental(), backupStrategyForm.getRepositories());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBackupStrategy(BackupStrategy backupStrategy) {
        String backupStrategyName = backupStrategy.getStrategyName();
        BackupStrategy existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyName).build());
        if (Objects.isNull(existsBackupStrategy)) {
            return;
        }
        clearBackupStrategyRepositoryCache(existsBackupStrategy.getId());
        Long backupStrategyId = existsBackupStrategy.getId();
        backupStrategyMapper.deleteByPrimaryKey(backupStrategyId);
        //删除仓库
        Example backupStrategyRepositoryExample = new Example(BackupStrategyRepository.class);
        backupStrategyRepositoryExample.createCriteria().andEqualTo("backupStrategyId", backupStrategyId);
        backupStrategyRepositoryMapper.deleteByExample(backupStrategyRepositoryExample);
        deleteCronTask(backupStrategyName, existsBackupStrategy.getCronExpression());
    }

    @Override
    public BackupStrategy getBackupStrategy(BackupStrategy backupStrategy) {
        return backupStrategyMapper.selectOne(backupStrategy);
    }

    @Override
    public List<BackupStrategyRecord> getBackupStrategyRecordCache(String storageId, String repositoryId) {
        String repositoryCacheKey = String.format("%s-%s", storageId, repositoryId);
        List<BackupStrategyRecord> backupStrategyRecordList = getCache(repositoryCacheKey);
        if (CollectionUtils.isEmpty(backupStrategyRecordList)) {
            backupStrategyRecordList = queryBackupStrategyList(BackupStrategyForm.builder().storageId(storageId).repositoryId(repositoryId).build());
            putCache(repositoryCacheKey, backupStrategyRecordList, 8);
        }
        return backupStrategyRecordList;
    }

    @Override
    @Async("asyncThreadPoolTaskExecutor")
    public void executeBackup(BackupStrategyForm backupStrategyForm) {
        BackupStrategy existsBackupStrategy = getBackupStrategy(BackupStrategy.builder().strategyName(backupStrategyForm.getStrategyName()).build());
        if (Objects.isNull(existsBackupStrategy)) {
            return;
        }
        try {
            backupComponent.backupRepositories(existsBackupStrategy.getStrategyName(), existsBackupStrategy.getBackupPath(), Boolean.TRUE.equals(existsBackupStrategy.getIncremental()), getRepositories(existsBackupStrategy.getId()));
        } catch (Exception ex) {
            log.error("Execute backup [{}] error [{}]", existsBackupStrategy.getStrategyName(), ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 刷新缓存
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     */
    private void clearCache(String storageId, String repositoryId) {
        String repositoryCacheKey = String.format("%s-%s", storageId, repositoryId);
        Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(REPOSITORY_BACKUP_STRATEGY_KEY);
        if (MapUtils.isNotEmpty(hazelcastMap)) {
            hazelcastMap.remove(repositoryCacheKey);
            log.info("Clear backup strategy cache [{}] [{}]", storageId, repositoryId);
        }
    }

    /**
     * 加入缓存
     *
     * @param key        key
     * @param cacheValue 缓存值
     * @param ttl        缓存时间，小时
     */
    private void putCache(String key, List<BackupStrategyRecord> cacheValue, long ttl) {
        if (CollectionUtils.isEmpty(cacheValue)) {
            return;
        }
        hazelcastInstance.getMap(REPOSITORY_BACKUP_STRATEGY_KEY).put(key, cacheValue, ttl, TimeUnit.HOURS);
    }

    /**
     * 获取缓存
     *
     * @param key key
     * @return 缓存值
     */
    public List<BackupStrategyRecord> getCache(String key) {
        try {
            Map<String, List<BackupStrategyRecord>> hazelcastMap = hazelcastInstance.getMap(REPOSITORY_BACKUP_STRATEGY_KEY);
            if (MapUtils.isEmpty(hazelcastMap)) {
                return null;
            }
            return hazelcastMap.get(key);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }

    /**
     * 清理备份策略缓存
     *
     * @param id 备份策略id
     */
    private void clearBackupStrategyRepositoryCache(Long id) {
        Example backupStrategyRepositoryExample = new Example(BackupStrategyRepository.class);
        backupStrategyRepositoryExample.createCriteria().andEqualTo("backupStrategyId", id);
        List<BackupStrategyRepository> existsBackupStrategyRepositories = backupStrategyRepositoryMapper.selectByExample(backupStrategyRepositoryExample);
        if (CollectionUtils.isNotEmpty(existsBackupStrategyRepositories)) {
            for (BackupStrategyRepository existsBackupStrategyRepository : existsBackupStrategyRepositories) {
                //清理上次保存的仓库缓存
                clearCache(existsBackupStrategyRepository.getStorageId(), existsBackupStrategyRepository.getRepositoryId());
            }
        }
    }

    private void createCronTask(String strategyName, Boolean enabled, String cronName, String cron, String existsCron, String backupPath, Boolean incremental, List<String> repositories) {
        if (Boolean.FALSE.equals(enabled) && StringUtils.isNotBlank(existsCron)) {
            deleteCronTask(cronName, existsCron);
            return;
        }
        if (Boolean.TRUE.equals(enabled) && StringUtils.isNotBlank(cron)) {
            Map<String, String> properties = Maps.newLinkedHashMap();
            properties.put(ArtifactBackupCronJob.PROPERTY_STRATEGY_NAME, strategyName);
            properties.put(ArtifactBackupCronJob.PROPERTY_STORAGE_ID, cronName);
            properties.put(ArtifactBackupCronJob.PROPERTY_REPOSITORY_ID, cronName);
            properties.put(ArtifactBackupCronJob.PROPERTY_BACKUP_PATH, backupPath);
            properties.put(ArtifactBackupCronJob.PROPERTY_INCREMENTAL, incremental.toString());
            for (String storageIdAndRepositoryId : repositories) {
                properties.put(storageIdAndRepositoryId, storageIdAndRepositoryId);
            }
            cronComponent.configCronTask(cronName, ArtifactBackupCronJob.class.getName(), cron, properties);
        }
    }

    private void deleteCronTask(String cronName, String cron) {
        cronComponent.deleteCronTask(cronName, ArtifactBackupCronJob.class.getName(), cron);
    }

    private List<String> getRepositories(Long backupStrategyId) {
        Example backupStrategyRepositoryExample = new Example(BackupStrategyRepository.class);
        backupStrategyRepositoryExample.createCriteria().andEqualTo("backupStrategyId", backupStrategyId);
        List<BackupStrategyRepository> backupStrategyRepositories = backupStrategyRepositoryMapper.selectByExample(backupStrategyRepositoryExample);
        return Optional.ofNullable(backupStrategyRepositories).orElse(Collections.emptyList()).stream().map(backupStrategyRepository -> ConfigurationUtils.getStorageIdAndRepositoryId(backupStrategyRepository.getStorageId(), backupStrategyRepository.getRepositoryId())).collect(Collectors.toList());
    }


}
