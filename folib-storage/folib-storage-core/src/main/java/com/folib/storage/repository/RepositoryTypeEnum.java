package com.folib.storage.repository;

import com.folib.api.Describable;

/**
 * @author veadan
 */
public enum RepositoryTypeEnum
        implements Describable {

    /**
     * 本地库
     */
    HOSTED("hosted", "本地库"),

    /**
     * 代理库
     */
    PROXY("proxy", "代理库"),

    /**
     * 组合库
     */
    GROUP("group", "组合库"),

    // Unsupported
    VIRTUAL("virtual", "virtual");


    private String type;

    private String name;

    RepositoryTypeEnum(String type, String name) {
        this.type = type;
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String describe() {
        return getType();
    }

    public static String queryName(String type) {
        for (RepositoryTypeEnum item : RepositoryTypeEnum.values()) {
            if (item.type.equals(type)) {
                return item.name;
            }
        }
        return "";
    }
}
