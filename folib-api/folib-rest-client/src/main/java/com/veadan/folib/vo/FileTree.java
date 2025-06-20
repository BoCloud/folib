package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FileTree {

    /**
     * 名称
     */
    private String name;
    /**
     * 类型
     */
    private String type;
    /**
     * isLeaf
     */
    private boolean isLeaf;
    /**
     * 子集
     */
    private List<FileTree> children;
}
