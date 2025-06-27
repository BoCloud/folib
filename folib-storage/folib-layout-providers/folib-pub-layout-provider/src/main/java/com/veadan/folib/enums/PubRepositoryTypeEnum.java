package com.veadan.folib.enums;

import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 条件类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum PubRepositoryTypeEnum {

    /**
     * pub_group
     */
    PUB_GROUP("pub_group", RepositoryTypeEnum.GROUP.getType()),
    /**
     * pub_hosted
     */
    PUB_HOSTED("pub_hosted", RepositoryTypeEnum.HOSTED.getType()),
    /**
     * pub_proxy
     */
    PUB_PROXY("pub_proxy", RepositoryTypeEnum.PROXY.getType()),
    ;

    /**
     * type
     */
    private String type;

    /**
     * repositoryType
     */
    private String repositoryType;

    public static String resolveType(String repositoryType) {
        String type = "";
        for (PubRepositoryTypeEnum item : PubRepositoryTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }

}
