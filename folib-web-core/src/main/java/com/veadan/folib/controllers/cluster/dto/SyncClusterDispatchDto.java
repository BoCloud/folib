package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SyncClusterDispatchDto {
    private ClusterDispatchNodeDto nodeDto;
    private SyncClusterDispatchEnum syncClusterDispatchEnum;
}
