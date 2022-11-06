package com.veadan.folib.scanner.biz;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.veadan.folib.scanner.common.base.BaseQuery;
import com.veadan.folib.scanner.common.base.BusinessBiz;
import com.veadan.folib.scanner.common.constant.ScanConstans;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.scanner.common.util.Query;
import com.veadan.folib.scanner.entity.*;
import com.veadan.folib.scanner.enums.SeverityTypeEnum;
import com.veadan.folib.scanner.mapper.FolibScannerMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import java.text.DecimalFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author Veadan
 * @version 2022-05-31 23:12:54
 * @email xuxinping@126.com
 */
@Service
public class FolibScannerBiz extends BusinessBiz<FolibScannerMapper, FolibScanner> {

    public List<FolibScanner> selectEnableScan() {
        return this.mapper.selectEnableScan();
    }

    public void updateScaning() {
        this.mapper.updateScaning();
    }

    public ScanSumVo getScanSum() {
        return this.mapper.getScanSum(getBaseQuery());
    }

    public JSONObject getTotalSum() {
        BaseQuery baseQuery = getBaseQuery();
        JSONObject object = new JSONObject();
        FolibScanner folibScanner = new FolibScanner();
        folibScanner.setOnScan(true);
        Integer onScanCount = this.mapper.selectFolibScannerCount(folibScanner, baseQuery);
        folibScanner.setOnScan(false);
        Integer notScanCount = this.mapper.selectFolibScannerCount(folibScanner, baseQuery);
        folibScanner.setScanStatus(ScanConstans.UNSCAN).setOnScan(true);
        Integer onScanAndUnScan = this.mapper.selectFolibScannerCount(folibScanner, baseQuery);
        folibScanner.setScanStatus(ScanConstans.SCANED).setOnScan(true);
        Integer onScanAndScaned = this.mapper.selectFolibScannerCount(folibScanner, baseQuery);
        folibScanner.setScanStatus(ScanConstans.SCANFAILED).setOnScan(true);
        Integer onScanAndScanFailed = this.mapper.selectFolibScannerCount(folibScanner, baseQuery);
        object.put("onScanCount", onScanCount);
        object.put("notScanCount", notScanCount);
        object.put("onScanAndUnScan", onScanAndUnScan);
        object.put("onScanAndScaned", onScanAndScaned);
        object.put("onScanAndScanFailed", onScanAndScanFailed);
        return object;

    }

    public List<ScannerSumDifVo> getScannerSumDifVoList() {
        List<ScannerSumDifVo> scannerSumDifVos = this.mapper.getScannerSumDifVoList(getBaseQuery());
        DecimalFormat decimalFormat = new DecimalFormat(".00");
        scannerSumDifVos.forEach(scannerSumDifVo -> {
            String r;
            if (scannerSumDifVo.getCountFolib() == 0) {
                r = "100";
            } else {
                r = decimalFormat.format((float) scannerSumDifVo.getVulnerableSum() / (float) scannerSumDifVo.getCountFolib() * 100);
            }
            double s = Double.parseDouble(r);
            int star = s == 100.0 ? 5 : s > 0 && s < 20 ? 4 : s > 20 && s < 40 ? 3 : s > 40 && s < 60 ? 2 : 1;
            scannerSumDifVo.setStar(star);
        });
        return scannerSumDifVos;
    }

    public JSONObject weekDayCount() {
        BaseQuery baseQuery = getBaseQuery();
        JSONObject object = new JSONObject();
        object.put("weekCount", this.mapper.weekDayCount(baseQuery));
        ScanSumByDate scanSumByDate = new ScanSumByDate();
        ScanSumByDate d14 = this.mapper.getCountByDayOne(14, baseQuery);
        ScanSumByDate d7 = this.mapper.getCountByDayOne(7, baseQuery);
        scanSumByDate.setDenpendencySum(d14.getDenpendencySum() - d7.getDenpendencySum());
        scanSumByDate.setCountFolib(d14.getCountFolib() - d7.getCountFolib());
        scanSumByDate.setSuppressedSum(d14.getSuppressedSum() - d7.getSuppressedSum());
        scanSumByDate.setVulnerabilitesSum(d14.getVulnerabilitesSum() - d7.getVulnerabilitesSum());
        scanSumByDate.setVulnerableSum(d14.getVulnerableSum() - d7.getVulnerableSum());
        object.put("compare", scanSumByDate);
        return object;
    }

    public List<ScanSumByDate> mounthDayCount() {
        return this.mapper.mounthDayCount(getBaseQuery());
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
            Query query1 = query;
            query1.remove("page");
            query1.remove("limit");
            for (Map.Entry<String, Object> entry : query1.entrySet()) {
                if (StringUtils.isBlank(entry.getValue().toString())) {
                    continue;
                }
                Example.Criteria criteria = example.createCriteria();
                if (entry.getKey().equals("artifactName")) {
                    criteria.andLike("path", "%" + entry.getValue().toString() + "%");
                } else {
                    criteria.andEqualTo(entry.getKey(), entry.getValue().toString());
                }
                example.and(criteria);
            }
        }
        BaseQuery baseQuery = getBaseQuery();
        Example.Criteria criteria = example.createCriteria();
        criteria.andNotEqualTo("vulnerableCount", 0);
        if (CollectionUtils.isNotEmpty(baseQuery.getStorageIdList())) {
            criteria.andIn("storage", baseQuery.getStorageIdList());
        }
        if (CollectionUtils.isNotEmpty(baseQuery.getNotInStorageIdList())) {
            criteria.andNotIn("storage", baseQuery.getNotInStorageIdList());
        }
        example.and(criteria);
    }

    /**
     * 获取制品的漏洞严重程度信息
     *
     * @param id 制品id
     * @return 制品的漏洞严重程度信息
     */
    public SeverityVO severity(String id) {
        Long zero = 0L;
        SeverityVO severityVO = SeverityVO.builder().critical(zero).high(zero).low(zero).medium(zero).show(false).build();
        FolibScanner folibScanner = this.mapper.selectByPrimaryKey(id);
        if (Objects.nonNull(folibScanner)) {
            BeanUtils.copyProperties(folibScanner, severityVO);
            if (ScanConstans.SCANED.equals(folibScanner.getScanStatus())) {
                //扫描完成才展示报告
                severityVO.setShow(true);
                List<VulnerabilityVO> list = JSONArray.parseArray(folibScanner.getReport(), VulnerabilityVO.class);
                List<String> severityList = Lists.newArrayList();
                list.stream().map(VulnerabilityVO::getVulnerabilities).forEach(item -> {
                    severityList.addAll(item.stream().map(HighestSeverityTextVO::getHighestSeverityText).collect(Collectors.toList()));
                });
                if (CollectionUtils.isNotEmpty(severityList)) {
                    Long critical = severityList.stream().filter(item -> SeverityTypeEnum.CRITICAL.getType().equals(item)).count();
                    Long high = severityList.stream().filter(item -> SeverityTypeEnum.HIGH.getType().equals(item)).count();
                    Long medium = severityList.stream().filter(item -> SeverityTypeEnum.MEDIUM.getType().equals(item)).count();
                    Long low = severityList.stream().filter(item -> SeverityTypeEnum.LOW.getType().equals(item)).count();
                    severityVO.setCritical(critical);
                    severityVO.setHigh(high);
                    severityVO.setMedium(medium);
                    severityVO.setLow(low);
                }
            }
        }
        return severityVO;
    }

    /**
     * 查询docker布局扫描报告
     *
     * @param params 参数
     * @return docker布局扫描报告
     */
    public TableResultResponse<FolibScannerDockerTableVO> dockerPage(Map<String, Object> params) {
        Query query = new Query(params);
        String storageKey = "storage", storage = "";
        if (query.containsKey(storageKey)) {
            storage = query.get(storageKey).toString();
        }
        String repositoryKey = "repository", repository = "";
        if (query.containsKey(repositoryKey)) {
            repository = query.get(repositoryKey).toString();
        }
        String artifactNameKey = "artifactName", artifactName = "";
        if (query.containsKey(artifactNameKey)) {
            artifactName = query.get(artifactNameKey).toString();
        }
        Page<Object> result = PageHelper.startPage(query.getPage(), query.getLimit());
        List<FolibScannerDockerTableVO> list = this.mapper.selectDockerList(storage, repository, artifactName);
        if (CollectionUtils.isNotEmpty(list)) {
            list.forEach(item -> {
                item.setPath(item.getVersionPath());
                if (StringUtils.isNotBlank(item.getVersionPath())) {
                    //查询子表数据
                    List<FolibScanner> childList = this.mapper.selectDockerChildList(item.getStorage(), item.getRepository(), item.getVersionPath());
                    item.setChildList(childList);
                }
            });
        }
        return new TableResultResponse<FolibScannerDockerTableVO>(result.getTotal(), list);
    }
}
