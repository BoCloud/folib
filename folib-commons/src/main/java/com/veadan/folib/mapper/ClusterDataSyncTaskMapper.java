package com.veadan.folib.mapper;

import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface ClusterDataSyncTaskMapper {

    int addTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

    int updateTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

    List<ClusterDataSyncTaskPo> getClusterDataSyncTaskList(@Param("status") Integer status, @Param("host") String host);
}
