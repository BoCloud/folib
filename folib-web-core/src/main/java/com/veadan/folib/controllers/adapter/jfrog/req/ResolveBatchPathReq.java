package com.veadan.folib.controllers.adapter.jfrog.req;

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
public class ResolveBatchPathReq {

    /**
     * files
     */
    @Valid
    @NotEmpty(message = "请传入files参数")
    private List<ResolvePathFiles> files;
}
