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
package com.folib.indexer;

import com.folib.storage.repository.Repository;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * @author veadan
 * @since 2025-03-07 10:13
 */
@Slf4j
@Data
public class DebianRepoGroupMetadataIndexFinalizer {

    private Repository groupRepo;
    private List<String> compFoldersToDelete;
    private String distribution;

    public DebianRepoGroupMetadataIndexFinalizer( Repository groupRepo, String distribution, List<String> packagesFilesToDelete) {
        this.distribution=distribution;
        this.compFoldersToDelete = packagesFilesToDelete;
        this.groupRepo = groupRepo;
    }
    public void finalizeIndex() {
//        String source = this.tempPathRoot + "/" + this.distribution + "/";
//        String target = "dists" + this.resolveParentDist(this.distribution);
//        long move = System.currentTimeMillis();
//        log.debug("Moving calculated index from temp location {} to location {}", source, target);
//        ((DebianRepoWorkContext)this.repo.getWorkContext()).setSystem();
//        ((DebianService)ContextHelper.get().beanForType(DebianService.class)).finalizeVirtualIndexing(this.repo, this.virtualRepo, source, target, this.distribution, this.compFoldersToDelete);
//        log.trace("Finished move to location {}. took {}ms", target, DpkgUtils.time(move));
    }

    public void removeTemp() {

    }
}
