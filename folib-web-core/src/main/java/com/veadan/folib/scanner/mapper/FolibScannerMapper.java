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
	
}
