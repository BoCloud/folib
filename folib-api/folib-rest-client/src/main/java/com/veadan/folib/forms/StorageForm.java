package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author veadan
 * @date 2022/11/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class StorageForm {

    /**
     * 存储空间名称
     */
    private String id;

    /**
     * 路径
     */
    private String basedir;

    /**
     * 管理员
     */
    private String admin;

    /**
     * 存储类型 local、s3
     */
    private String storageProvider;

    /**
     * 存储配额
     */
    private Long storageMaxSize;

    /**
     * 普通用户
     */
    private Set<String> users;
}
