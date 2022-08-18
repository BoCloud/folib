package com.veadan.folib.scanner.biz;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.scanner.common.base.BusinessBiz;
import com.veadan.folib.scanner.common.constant.ScanConstans;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.scanner.common.util.Query;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.entity.ScanSumByDate;
import com.veadan.folib.scanner.entity.ScanSumVo;
import com.veadan.folib.scanner.entity.ScannerSumDifVo;
import com.veadan.folib.scanner.mapper.FolibScannerMapper;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import java.text.NumberFormat;
import java.util.List;
import java.util.Map;

/**
 * 
 *
 * @author Veadan
 * @email xuxinping@126.com
 * @version 2022-05-31 23:12:54
 */
@Service
public class FolibScannerBiz extends BusinessBiz<FolibScannerMapper,FolibScanner> {

    public List<FolibScanner>  selectEnableScan(){
        return this.mapper.selectEnableScan();
    }
    public void updateScaning(){
        this.mapper.updateScaning();
    }

    public ScanSumVo getScanSum(){
        return this.mapper.getScanSum();
    }

    public JSONObject getTotalSum(){
        JSONObject object =new JSONObject();
        FolibScanner folibScanner = new FolibScanner();
        folibScanner.setOnScan(true);
        Integer onScanCount = this.mapper.selectCount(folibScanner);
        folibScanner.setOnScan(false);
        Integer notScanCount = this.mapper.selectCount(folibScanner);
        folibScanner.setScanStatus(ScanConstans.UNSCAN).setOnScan(true);
        Integer  onScanAndUnScan=this.mapper.selectCount(folibScanner);
        folibScanner.setScanStatus(ScanConstans.SCANED).setOnScan(true);
        Integer  onScanAndScaned=this.mapper.selectCount(folibScanner);
        folibScanner.setScanStatus(ScanConstans.SCANFAILED).setOnScan(true);
        Integer  onScanAndScanFailed=this.mapper.selectCount(folibScanner);
        object.put("onScanCount",onScanCount);
        object.put("notScanCount",notScanCount);
        object.put("onScanAndUnScan",onScanAndUnScan);
        object.put("onScanAndScaned",onScanAndScaned);
        object.put("onScanAndScanFailed",onScanAndScanFailed);
        return object;

    }

    public List<ScannerSumDifVo>  getScannerSumDifVoList(){
        List<ScannerSumDifVo> scannerSumDifVos = this.mapper.getScannerSumDifVoList();
        NumberFormat numberformat=NumberFormat.getInstance();
        numberformat.setMaximumFractionDigits(2);
        scannerSumDifVos.forEach(scannerSumDifVo -> {
            String r ;
            if(scannerSumDifVo.getCountFolib()==0){
                r="100";
            }else {
                r =numberformat.format((float)scannerSumDifVo.getVulnerableSum()/(float)scannerSumDifVo.getCountFolib()*100);
            }
            double s= Double.parseDouble(r);
            int star = s==100.0?5:s>0&&s<20?4:s>20&&s<40?3:s>40&&s<60?2:1;
           scannerSumDifVo.setStar(star);
        });
        return scannerSumDifVos;
    }

    public JSONObject weekDayCount(){
        JSONObject object = new JSONObject();
        object.put("weekCount",this.mapper.weekDayCount());
        ScanSumByDate scanSumByDate=new ScanSumByDate();
        ScanSumByDate d14= this.mapper.getCountByDayOne(14);
        ScanSumByDate d7= this.mapper.getCountByDayOne(7);
        scanSumByDate.setDenpendencySum(d14.getDenpendencySum()-d7.getDenpendencySum());
        scanSumByDate.setCountFolib(d14.getCountFolib()-d7.getCountFolib());
        scanSumByDate.setSuppressedSum(d14.getSuppressedSum()-d7.getSuppressedSum());
        scanSumByDate.setVulnerabilitesSum(d14.getVulnerabilitesSum()-d7.getVulnerabilitesSum());
        scanSumByDate.setVulnerableSum(d14.getVulnerableSum()-d7.getVulnerableSum());
        object.put("compare",scanSumByDate);
        return object;
    }

    public  List<ScanSumByDate> mounthDayCount(){
        return this.mapper.mounthDayCount();
    }

    @Override
    public TableResultResponse<FolibScanner> selectByQuery(Query query) {
        TableResultResponse<FolibScanner> tableResultResponse = super.selectByQuery(query);
        tableResultResponse.getData().getRows().forEach(folibScanner -> folibScanner.setReport(null));
        return tableResultResponse;
    }

    @Override
    public void query2criteria(Query query, Example example) {
        if (query.entrySet().size() > 0) {
            Query query1 =query;
            query1.remove("page");
            query1.remove("limit");
            for (Map.Entry<String, Object> entry : query1.entrySet()) {
                Example.Criteria criteria = example.createCriteria();
                criteria.andEqualTo(entry.getKey(), entry.getValue().toString());
                example.and(criteria);
            }
        }
                Example.Criteria criteria = example.createCriteria();
                criteria.andNotEqualTo("vulnerableCount",0);
                example.and(criteria);
    }
}
