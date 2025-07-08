package com.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.entity.GitLfsLockEntity;
import org.springframework.stereotype.Component;
import org.apache.ibatis.annotations.Param;
import java.util.List;

@Component
public interface GitLfsLockMapper extends BaseMapper<GitLfsLockEntity> {

    /**
     * 分页查询指定行数据
     *
     * @return 对象列表
     */
    List<GitLfsLockEntity> queryAllByLimit(@Param("storageId") String storageId,
                                           @Param("repositoryId") String repositoryId,
                                           @Param("path") String path,
                                           @Param("id") String id,
                                           @Param("offset") int cursor,
                                           @Param("pageSize") int pageSize,
                                           @Param("ref") String refSpec);
}
