package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SyncClusterDispatchDto implements Serializable {
    private ClusterDispatchNodeDto nodeDto;
    private SyncClusterDispatchEnum syncClusterDispatchEnum;
}
