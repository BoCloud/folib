package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品同步
 *
 * @author leipenghui
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum WebhookEventsTypeEnum {

    /**
     * GENERAL
     */
    GENERAL("WEBHOOK_EVENTS_GENERAL", "GENERAL"),
    /**
     * NPM
     */
    NPM("WEBHOOK_EVENTS_NPM", ProductTypeEnum.Npm.getFoLibraryName()),
    /**
     * DOCKER
     */
    DOCKER("WEBHOOK_EVENTS_DOCKER", ProductTypeEnum.Docker.getFoLibraryName()),
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
        String type = WebhookEventsTypeEnum.GENERAL.type;
        for (WebhookEventsTypeEnum item : WebhookEventsTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }
}
