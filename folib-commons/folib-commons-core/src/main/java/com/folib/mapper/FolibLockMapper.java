package com.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.entity.FolibLock;
import org.apache.ibatis.annotations.Param;

public interface FolibLockMapper extends BaseMapper<FolibLock> {
    int insertLock(@Param("name") String name, @Param("lockUntil") String lockUntil,
                   @Param("lockedAt") String lockedAt, @Param("lockedBy") String lockedBy);

    FolibLock selectFolibLock(@Param("name") String name);

    int deleteFolibLock(@Param("name") String name);

    int updateFolibLock(@Param("name") String name, @Param("lockUntil") String lockUntil,
                        @Param("lockedAt") String lockedAt, @Param("lockedBy") String lockedBy);

}
