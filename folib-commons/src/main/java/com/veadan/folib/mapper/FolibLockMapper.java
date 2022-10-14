package com.veadan.folib.mapper;

import com.veadan.folib.entity.FolibLock;
import org.apache.ibatis.annotations.Param;

public interface FolibLockMapper{
    int insertLock(@Param("name") String name, @Param("lockUntil") String lockUntil,
                   @Param("lockedAt") String lockedAt, @Param("lockedBy") String lockedBy);

    FolibLock selectFolibLock(@Param("name") String name);

    int deleteFolibLock(@Param("name") String name);

    int updateFolibLock(@Param("name") String name, @Param("lockUntil") String lockUntil,
                        @Param("lockedAt") String lockedAt, @Param("lockedBy") String lockedBy);

}
