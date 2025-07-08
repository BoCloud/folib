package com.folib.forms.backup;

import com.folib.forms.common.RepositoryForm;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author leipenghui
 * @date 2023/9/27
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BackupForm {

    /**
     * 备份仓库列表
     */
    @Valid
    @NotEmpty
    private List<RepositoryForm> repositoryList;

    /**
     * 备份目录
     */
    @NotBlank
    private String directoryPath;
}
