package com.veadan.folib.model.response;

import io.swagger.annotations.ApiModel;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;
import org.checkerframework.checker.units.qual.A;

@Data
@Builder
@Accessors(chain = true)
@ApiModel(value = "OhpmPublishRes",description = "ohpm发布结果")
public class OhpmPublishRes {

    private String additionalMsg;
    private boolean success;
}
