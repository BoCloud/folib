package com.veadan.folib.enums;

import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.Getter;

/**
 * 条件类型枚举
 *
 * @author huayanjun
 **/
@Getter
public enum DebianRepositoryTypeEnum {

    /**
     * pub_group
     */
    DEBIAN_GROUP("debian_group", RepositoryTypeEnum.GROUP.getType()),
    /**
     * pub_hosted
     */
    DEBIAN_HOSTED("debian_hosted", RepositoryTypeEnum.HOSTED.getType()),
    /**
     * pub_proxy
     */
    DEBIAN_PROXY("debian_proxy", RepositoryTypeEnum.PROXY.getType()),
    ;

    /**
     * type
     */
    private final String type;

    /**
     * repositoryType
     */
    private final String repositoryType;

    DebianRepositoryTypeEnum(String type, String repositoryType) {
        this.type = type;
        this.repositoryType = repositoryType;
    }

    public static String resolveType(String repositoryType) {
        String type = "";
        for (DebianRepositoryTypeEnum item : DebianRepositoryTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }

}
