package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author veadan
 * @date 2024/12/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ResolveBatchPath {

    /**
     * files
     */
    @Valid
    @NotEmpty(message = "请传入files参数")
    private List<ResolvePathFiles> files;
}
