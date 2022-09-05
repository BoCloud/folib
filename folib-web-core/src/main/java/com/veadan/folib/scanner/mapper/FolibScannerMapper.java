package com.veadan.folib.scanner.mapper;

import com.veadan.folib.scanner.common.base.CommonMapper;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.entity.ScanSumByDate;
import com.veadan.folib.scanner.entity.ScanSumVo;
import com.veadan.folib.scanner.entity.ScannerSumDifVo;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 
 * 
 * @author Veadan
 * @email xuxinping@126.com
 * @version 2022-05-31 23:12:54
 */
public interface FolibScannerMapper extends CommonMapper<FolibScanner> {
    //=#{enableScan} where repository=#{repository} and storage=#{storage}
    public void updateByStorage(@Param("enableScan") boolean enableScan,@Param("repository") String repository,@Param("storage") String storage);

    List<FolibScanner> selectEnableScan();
    void updateScaning();

    ScanSumVo getScanSum();
    List<ScannerSumDifVo>  getScannerSumDifVoList();
    List<ScanSumByDate> mounthDayCount();

    List<ScanSumByDate> weekDayCount();

    ScanSumByDate getCountByDayOne(@Param("dateNum") int dateNum);

    /**
     * 根据制品目录进行批量删除
     * @param storage 存储空间
     * @param repository 仓库名称
     * @param path 目录
     */
    void deleteByPathLike(@Param("storage") String storage, @Param("repository") String repository, @Param("path") String path);
	
}
