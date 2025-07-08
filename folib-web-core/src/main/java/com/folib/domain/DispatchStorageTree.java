package com.folib.domain;

import com.folib.forms.common.StorageTreeForm;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

/**
 * 三级联动实体
 *
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DispatchStorageTree {
    private List<StorageTreeForm> list;
}
