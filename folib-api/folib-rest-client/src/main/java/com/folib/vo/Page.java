package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/28
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Page<T> {

    private List<T> list;

    private Long total;
}
