package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerRootFs {

    /**
     * diffIds
     */
    private List<String> diffIds;

    /**
     * 类型
     */
    private String type;
}
