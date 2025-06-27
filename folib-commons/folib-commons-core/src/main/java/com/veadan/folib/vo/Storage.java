package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Storage {

    /**
     * 存储空间名称
     */
    private String id;

    /**
     * 存储空间路径
     */
    private String basedir;

    /**
     * 存储空间下的仓库列表
     */
    private List<Repository> repositories;
}
