package com.veadan.folib.dto.backup;

import com.veadan.folib.dto.common.RepositoryDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @author veadan
 * @date 2023/9/27
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BackupDto {

    /**
     * 备份仓库列表
     */
    @Valid
    @NotEmpty
    private List<RepositoryDto> repositoryList;

    /**
     * 备份目录
     */
    @NotBlank
    private String directoryPath;
}
