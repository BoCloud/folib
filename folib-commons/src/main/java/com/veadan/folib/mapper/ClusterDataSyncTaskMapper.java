package com.veadan.folib.mapper;

import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public interface ClusterDataSyncTaskMapper {

    int addTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

    int updateTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

    List<ClusterDataSyncTaskPo> getClusterDataSyncTaskList(@Param("status") Integer status, @Param("host") String host);
}
