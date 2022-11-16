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
public class DockerRootFs {

    /**
     * diffIds
     */
    @JSONField(name = "diff_ids")
    private List<String> diffIds;

    /**
     * 类型
     */
    private String type;
}
