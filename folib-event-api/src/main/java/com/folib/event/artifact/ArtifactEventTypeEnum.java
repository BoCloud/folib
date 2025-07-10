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
package com.folib.event.artifact;

/**
 * @author Veadan
 */
public enum ArtifactEventTypeEnum {

    /**
     * Occurs when a directory has been created.
     * <p>
     * TODO: Not yet implemented.
     */
    EVENT_ARTIFACT_DIRECTORY_CREATED(1),

    /**
     * Occurs when an artifact has commenced an upload operation.
     */
    EVENT_ARTIFACT_FILE_UPLOADING(2),

    /**
     * Occurs when an artifact metadata file has been stored.
     */
    EVENT_ARTIFACT_METADATA_STORED(7),

    /**
     * Occurs when an artifact metadata file has been updated.
     */
    EVENT_ARTIFACT_FILE_DOWNLOADING(8),

    /**
     * Occurs when an artifact download operation has completed.
     */
    EVENT_ARTIFACT_FILE_DOWNLOADED(9),

    /**
     * Occurs when an artifact file has been updated.
     */
    EVENT_ARTIFACT_FILE_UPDATED(10),

    /**
     * Occurs when an artifact has commenced an copy operation.
     */
    EVENT_ARTIFACT_FILE_COPYING(11),

    /**
     * Occurs when an artifact copy operation has completed.
     */
    EVENT_ARTIFACT_FILE_COPIED(12),

    /**
     * Occurs when an artifact has commenced a move operation.
     */
    EVENT_ARTIFACT_FILE_MOVING(13),

    /**
     * Occurs when an artifact file has been moved.
     */
    EVENT_ARTIFACT_FILE_MOVED(14),

    /**
     * Occurs when an artifact file has commenced a archived archiving operation.
     */
    EVENT_ARTIFACT_FILE_ARCHIVING(15),

    /**
     * Occurs when an artifact file has been archived.
     */
    EVENT_ARTIFACT_FILE_ARCHIVED(15),

    /**
     * Occurs when an artifact file has been deleted.
     */
    EVENT_ARTIFACT_PATH_DELETED(16),

    /**
     * Occurs when an artifact metadata download operation has completed.
     */
    EVENT_ARTIFACT_METADATA_DOWNLOADED(17),

    /**
     * Occurs when an artifact metadata has commenced an download operation.
     */
    EVENT_ARTIFACT_METADATA_DOWNLOADING(18),

    /**
     * Occurs when an artifact checksum download operation has completed.
     */
    EVENT_ARTIFACT_CHECKSUM_DOWNLOADED(19),

    /**
     * Occurs when an artifact checksum has commenced an download operation.
     */
    EVENT_ARTIFACT_CHECKSUM_DOWNLOADING(20),

    /**
     * Occurs when an artifact has been fetched from the remote repository.
     */
    EVENT_ARTIFACT_FILE_FETCHED_FROM_REMOTE(21),

    /**
     * Occurs when an artifact file has been physically stored.
     */
    EVENT_ARTIFACT_FILE_STORED(22),
    /**
     * Occurs when an artifact directory has been deleted.
     */
    EVENT_ARTIFACT_DIRECTORY_PATH_DELETED(23),
    /**
     * Occurs when an metadata operation has update.
     */
    EVENT_ARTIFACT_METADATA_UPDATE(24),
    /**
     * Occurs when an artifact download operation has blocked.
     */
    EVENT_ARTIFACT_FILE_DOWNLOAD_BLOCKED(25),
    /**
     * Occurs when an artifact cache operation has need.
     */
    EVENT_ARTIFACT_FILE_CACHE(26),

    /**
     * Occurs when an artifact promotion operation has completed.
     */
    EVENT_ARTIFACT_FILE_PROMOTION(27),
    /**
     * Occurs when an artifact dispense operation has completed.
     */
    EVENT_ARTIFACT_FILE_DISPENSE(28)
    ;


    private int type;


    ArtifactEventTypeEnum(int type) {
        this.type = type;
    }

    public int getType() {
        return type;
    }

    /**
     * 根据类型查询枚举类型
     *
     * @param type 类型
     * @return 枚举类型
     */
    public static ArtifactEventTypeEnum queryArtifactEventTypeEnumByType(int type) {
        for (ArtifactEventTypeEnum artifactEventTypeEnum : ArtifactEventTypeEnum.values()) {
            if (artifactEventTypeEnum.getType() == type) {
                return artifactEventTypeEnum;
            }
        }
        return null;
    }
}
