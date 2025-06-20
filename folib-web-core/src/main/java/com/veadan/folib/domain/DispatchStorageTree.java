package com.veadan.folib.domain;

import com.veadan.folib.dto.common.StorageTreeDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

/**
 * 三级联动实体
 *
 * @author qijianping
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DispatchStorageTree {
    private List<StorageTreeDto> list;
}
