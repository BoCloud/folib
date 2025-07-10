/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.providers.io;

import com.folib.event.AsyncEventListener;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.folib.util.ThrowingPredicate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author veadan
 * @date 2024/1/19
 **/
@Slf4j
@Component
public class DockerProxyRepositoryPathFetchEventListener {

    @Inject
    private List<DockerExpiredRepositoryPathHandler> expiredRepositoryPathHandlers;

    @AsyncEventListener
    public void handle(final ProxyRepositoryPathExpiredEvent event) {

        RepositoryPath repositoryPath = event.getPath();
        if (!DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout())) {
            return;
        }

        expiredRepositoryPathHandlers.stream()
                .filter(ThrowingPredicate.unchecked((handler) -> handler.supports(repositoryPath)))
                .forEach(handleExpiration(repositoryPath));
    }

    private Consumer<DockerExpiredRepositoryPathHandler> handleExpiration(final RepositoryPath repositoryPath) {
        return handler ->
        {
            try {
                handler.handleExpiration(repositoryPath);
            } catch (IOException e) {
                log.error("Expired path [{}] improperly handled.", repositoryPath, e);
            }
        };
    }
}
