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
package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import com.folib.domain.RpmPackageArch;
import com.folib.domain.RpmPackageType;
import org.springframework.stereotype.Component;

@Component("rpmCoordinatesStrategy")
public class RpmCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {

        if (entity == null || entity.getCoordinates() == null) return null;
        RpmPackageType type = RpmPackageType.valueOf(entity.getCoordinates().get(RpmCoordinates.PACKAGE_TYPE));
        RpmCoordinates rpmArtifactCoordinates = null;
        if (entity.getCoordinates().containsKey(RpmCoordinates.ARCHITECTURE)) {

            rpmArtifactCoordinates = new RpmCoordinates(
                    entity.getCoordinates().get(RpmCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmCoordinates.VERSION),
                    entity.getCoordinates().get(RpmCoordinates.RELEASE),
                    type,
                    RpmPackageArch.valueOf(entity.getCoordinates().get(RpmCoordinates.ARCHITECTURE)),
                    entity.getCoordinates().get(RpmCoordinates.NAME)
            );
        } else {
            rpmArtifactCoordinates = new RpmCoordinates(
                    entity.getCoordinates().get(RpmCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmCoordinates.VERSION),
                    entity.getCoordinates().get(RpmCoordinates.RELEASE),
                    entity.getCoordinates().get(RpmCoordinates.NAME),
                    RpmPackageType.valueOf(entity.getCoordinates().get(RpmCoordinates.PACKAGE_TYPE))
            );
        }
        rpmArtifactCoordinates.setUuid(entity.getUuid());
        rpmArtifactCoordinates.setNativeId(entity.getNativeId());
        rpmArtifactCoordinates.setVersion(entity.getVersion());
        rpmArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(rpmArtifactCoordinates);

        return rpmArtifactCoordinates;
    }
}
