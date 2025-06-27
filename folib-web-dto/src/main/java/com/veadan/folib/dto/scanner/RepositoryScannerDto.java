package com.veadan.folib.dto.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryScannerDto {

    /**
     * 总数
     */
    private Long total;

    /**
     * 数据
     */
    private List<RepositoryDto> list;

}
