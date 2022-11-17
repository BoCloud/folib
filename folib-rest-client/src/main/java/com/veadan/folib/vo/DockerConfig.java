package com.veadan.folib.vo;

import com.alibaba.fastjson.annotation.JSONField;
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
public class DockerConfig {

    /**
     * argsEscaped
     */
    @JSONField(name = "ArgsEscaped")
    private Boolean argsEscaped;
    /**
     * cmd
     */
    @JSONField(name = "Cmd")
    private List<String> cmd;
    /**
     * env
     */
    @JSONField(name = "Env")
    private List<String> env;
    /**
     * onBuild
     */
    @JSONField(name = "OnBuild")
    private String onBuild;
}
