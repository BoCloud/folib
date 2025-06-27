package com.veadan.folib.components.webhook;


import com.veadan.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 **/
public interface WebhookEventsProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 处理webhook事件
     *
     * @param webhookDto          webhook信息
     * @param repositoryPath      制品信息
     * @param artifactMigrateInfo 同步源信息
     * @param type                类型 1 webhook 2 定时重试
     * @return 结果 true 成功 false 失败
     */
    boolean handler(WebhookDto webhookDto, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type);

    /**
     * 解析路径
     *
     * @param webhookDto webhook信息
     * @return path
     */
    String resolvePath(WebhookDto webhookDto);
}
