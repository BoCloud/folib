package com.veadan.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;

/**
 * 节点之间的晋级
 *
 * @author qijianping
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PromotionNodeOption {

    @NotEmpty
    private String sourcePath;

    @NotEmpty
    private String targetPath;

    private Integer syncModel;

}
