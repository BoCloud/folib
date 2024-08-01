package com.veadan.folib.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FileSizeStatisticsRes {

    private BigDecimal fileSize;

    private String repositoryId;
}
