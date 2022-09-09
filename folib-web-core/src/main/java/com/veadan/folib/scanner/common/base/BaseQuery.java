package com.veadan.folib.scanner.common.base;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * @author leipenghui
 * @date 2022/9/8
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BaseQuery implements Serializable {

    /**
     * 有权限存储空间id列表
     */
    private List<String> storageIdList;

    /**
     * 无权限存储空间id列表
     */
    private List<String> notInStorageIdList;
}
