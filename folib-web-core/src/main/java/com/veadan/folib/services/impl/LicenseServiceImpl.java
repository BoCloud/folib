package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Maps;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.domain.block.AllowlistDenylistBlockService;
import com.veadan.folib.domain.license.LicenseBlackWhite;
import com.veadan.folib.entity.AllowlistDenylistBlock;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.BlockDomainEnum;
import com.veadan.folib.enums.CategoryEnum;
import com.veadan.folib.enums.RuleEnum;
import com.veadan.folib.forms.license.LicenseTableForm;
import com.veadan.folib.mapper.LicenseMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.LicenseService;
import com.veadan.folib.utils.Translate;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class LicenseServiceImpl implements LicenseService {

    private final static String LICENSE_CACHE = "LICENSE_CACHE";

    private final static String LICENSE_CACHE_KEY = "LICENSE_CACHE_KEY";

    @Autowired
    private LicenseMapper licenseMapper;

    @Autowired
    @Lazy
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private HazelcastInstance hazelcastInstance;

    @Inject
    private AllowlistDenylistBlockService allowlistDenylistBlockService;

    /**
     * 将字符串按照指定长度分割成字符串数组
     *
     * @param src
     * @param length
     * @return
     */
    public static String[] stringToStringArray(String src, int length) {
        int len = src.length();
        int startIndex = 0;
        int endIndex = length;
        int numChunks = (int) Math.ceil((double) len / length); // 计算需要的子字符串数量
        String[] chunks = new String[numChunks];
        for (int i = 0; i < numChunks; i++) {
            if (endIndex > len) {
                endIndex = len;
            }
            String substring = src.substring(startIndex, endIndex);
            chunks[i] = substring;
            startIndex = endIndex;
            endIndex += length;
        }
        return chunks;

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveLicense(License license) {
        License dbLicense = selectOneLicense(license);
        if (Objects.nonNull(dbLicense)) {
            updateLicense(license);
        } else {
            licenseMapper.insertSelective(license);
        }
        clearCache();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateLicense(License license) {
        License dbLicense = selectOneLicense(license);
        if (Objects.nonNull(dbLicense)) {
            Example example = Example.builder(License.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("licenseId", license.getLicenseId());
            licenseMapper.updateByExampleSelective(license, example);
        }
        clearCache();
    }

    @Override
    public List<License> selectLicense(License license) {
        Example example = Example.builder(License.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (Objects.nonNull(license)) {
            if (Objects.nonNull(license.getIsDeprecated())) {
                criteria.andEqualTo("isDeprecated", license.getIsDeprecated());
            }
        }
        example.selectProperties("id", "licenseId", "licenseUrl");
        example.setOrderByClause("id");
        return licenseMapper.selectByExample(example);
    }

    @Override
    public License selectOneLicense(License license) {
        Example example = Example.builder(License.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("licenseId", license.getLicenseId());
        return licenseMapper.selectOneByExample(example);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateLicenseUrl() {
        List<License> licenseList = selectLicense(License.builder().build());
        if (CollectionUtils.isEmpty(licenseList)) {
            return;
        }
        int size = licenseList.size();
        Map<String, String> headerMap = Maps.newHashMap();
        headerMap.put("X-Api-Key", "F3PvIyiAn8txkToGV8N8JfWphnfd57i8");
        Response response = null;
        String url = "", res = "", licenseUrl = "";
        JSONObject data = null;
        License license, updateLicense;
        for (int i = 0; i < size; i++) {
            license = licenseList.get(i);
            url = "http://192.168.5.8:8081/api/v1/license/%s";
            url = String.format(url, license.getLicenseId());
            log.info("[{}] 许可证id [{}] 请求地址 [{}] 当前第 [{}] 个，剩余 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), url, (i + 1), size - (i + 1));
            try {
                response = doGet(url, headerMap);
                res = response.readEntity(String.class);
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 请求响应 [{}] ", this.getClass().getSimpleName(), license.getLicenseId(), url, res);
                data = JSONObject.parseObject(res);
                licenseUrl = data.getString("seeAlso");
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 许可证地址 [{}] ", this.getClass().getSimpleName(), license.getLicenseId(), url, licenseUrl);
                if (StringUtils.isNotBlank(licenseUrl)) {
                    licenseUrl = StringUtils.strip(licenseUrl, "[]");
                    updateLicense = License.builder().licenseId(license.getLicenseId()).licenseUrl(licenseUrl).build();
                    updateLicense(updateLicense);
                }
            } catch (Exception ex) {
                log.info("[{}] 许可证id [{}] 请求地址 [{}] 请求响应 [{}] 错误 [{}]", this.getClass().getSimpleName(), license.getLicenseId(), url, res, ExceptionUtils.getStackTrace(ex));
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateContentCn() {
        List<License> licenseList = selectLicense(License.builder().build());
        if (CollectionUtils.isEmpty(licenseList)) {
            return;
        }
        int size = licenseList.size();
        License license, updateLicense;
        String contentCn = "";
        Long total = 0L;
        for (int i = 0; i < licenseList.size(); i++) {
            license = licenseList.get(i);
            log.info("[{}] 许可证id [{}] 当前第 [{}] 个，剩余 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), (i + 1), size - (i + 1));
            try {
//                if (StringUtils.isNotBlank(license.getContent())) {
//                    log.info("[{}] 许可证id [{}] 当前第 [{}] 个，长度 [{}] 个", this.getClass().getSimpleName(), license.getLicenseId(), (i + 1), license.getContent().length());
//                    total = total + license.getContent().length();
//                }
                if (StringUtils.isNotBlank(license.getContent())) {
                    int length = license.getContent().length();
                    if (length > 2000) {
                        StringBuilder stringBuilder = new StringBuilder();
                        String[] arr = stringToStringArray(license.getContent(), 2000);
                        for (int j = 0; j < arr.length; j++) {
                            String content = arr[j];
                            contentCn = Translate.translate(content);
                            stringBuilder.append(contentCn);
                        }
                        contentCn = stringBuilder.toString();
                    } else {
                        contentCn = Translate.translate(license.getContent());
                    }
                }
                if (StringUtils.isNotBlank(contentCn)) {
                    updateLicense = License.builder().licenseId(license.getLicenseId()).contentCn(contentCn).build();
                    updateLicense(updateLicense);
                }
            } catch (Exception ex) {
                log.info("[{}] 许可证id [{}] 错误 [{}]", this.getClass().getSimpleName(), license.getLicenseId(), ExceptionUtils.getStackTrace(ex));
            }
        }
//        log.info("[{}] 许可证一共 [{}] 个字符", this.getClass().getSimpleName(), total);
    }

    @Override
    public TableResultResponse<LicenseTableForm> queryLicensePage(Integer page, Integer limit, String searchKeyword, String licenseId, Integer blackWhiteType) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<License> licenseList = licenseMapper.selectLicense(searchKeyword, licenseId, blackWhiteType, null, null);

        AllowlistDenylistBlock block = AllowlistDenylistBlock.builder()
                .category(CategoryEnum.LICENSE.toString())
                .domain(BlockDomainEnum.PLATFORM.toString())
                .build();

        List<AllowlistDenylistBlock>  list = allowlistDenylistBlockService.queryAllowlistDenylistBlockList(block);
        Set<String>  blackList = list.stream().filter(item-> RuleEnum.BLACKLIST.toString().equals(item.getType())).map(AllowlistDenylistBlock::getIdentifier).collect(Collectors.toSet());
        Set<String>  whiteList = list.stream().filter(item-> RuleEnum.WHITES.toString().equals(item.getType())).map(AllowlistDenylistBlock::getIdentifier).collect(Collectors.toSet());

        for (License  item : licenseList){
           if(blackList.stream().anyMatch(data-> item.getLicenseId().equals(data))){
               item.setBlackWhiteType(2);
           }else if(whiteList.stream().anyMatch(data-> item.getLicenseId().equals(data))){
               item.setBlackWhiteType(1);
           }else{
               item.setBlackWhiteType(0);
           }
        }

        return new TableResultResponse<LicenseTableForm>(result.getTotal(), Optional.ofNullable(licenseList).orElse(Collections.emptyList()).stream().map(license -> {
            LicenseTableForm licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
            return licenseTableForm;
        }).collect(Collectors.toList()));
    }

    @Override
    public List<LicenseTableForm> queryLicense(String searchKeyword, String licenseId, Integer blackWhiteType, Integer excludeBlackWhiteType) {
        List<License> licenseList = licenseMapper.selectLicense(searchKeyword, licenseId, blackWhiteType, excludeBlackWhiteType, null);
        return Optional.ofNullable(licenseList).orElse(Collections.emptyList()).stream().map(license -> {
            LicenseTableForm licenseTableForm = LicenseTableForm.builder().build();
            BeanUtils.copyProperties(license, licenseTableForm);
            return licenseTableForm;
        }).collect(Collectors.toList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void blackWhite(LicenseBlackWhite licenseBlackWhite) {
        License license = selectOneLicense(License.builder().licenseId(licenseBlackWhite.getLicenseId()).build());
        if (Objects.isNull(license)) {
            return;
        }
        Example example = Example.builder(License.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("licenseId", license.getLicenseId());
        licenseMapper.updateByExampleSelective(License.builder().blackWhiteType(licenseBlackWhite.getBlackWhiteType()).build(), example);
        clearCache();
    }

    @Override
    public List<License> getLicenseCache() {
        List<License> licenses = getCache();
        if (CollectionUtils.isEmpty(licenses)) {
            licenses = licenseMapper.selectLicense(null, null, null, null, 0);
            putCache(licenses, 8);
        }
        return licenses;
    }

    /**
     * 发送get请求
     *
     * @param url       url
     * @param headerMap headerMap
     * @return 响应
     */
    private Response doGet(String url, Map<String, String> headerMap) {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request(MediaType.APPLICATION_JSON);
        for (Map.Entry<String, String> entry : headerMap.entrySet()) {
            builder = builder.header(entry.getKey(), entry.getValue());
        }
        return builder.get();
    }

    /**
     * 刷新缓存
     */
    private void clearCache() {
        Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(LICENSE_CACHE_KEY);
        hazelcastMap.remove(LICENSE_CACHE);
    }

    /**
     * 加入缓存
     *
     * @param cacheValue 缓存值
     * @param ttl        缓存时间，小时
     */
    private void putCache(List<License> cacheValue, long ttl) {
        if (CollectionUtils.isEmpty(cacheValue)) {
            return;
        }
        hazelcastInstance.getMap(LICENSE_CACHE_KEY).put(LICENSE_CACHE, cacheValue, ttl, TimeUnit.HOURS);
    }

    /**
     * 获取缓存
     *
     * @return 缓存值
     */
    private List<License> getCache() {
        try {
            Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(LICENSE_CACHE_KEY);
            return hazelcastMap.get(LICENSE_CACHE);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }
}
