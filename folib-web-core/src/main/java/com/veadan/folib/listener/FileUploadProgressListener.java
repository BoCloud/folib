package com.veadan.folib.listener;

import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.fileupload.ProgressListener;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;

/**
 * @author leipenghui
 * @date 2023/2/12
 **/
@Slf4j
@Component
public class FileUploadProgressListener implements ProgressListener {

    @Autowired
    private DictService dictService;

    @Autowired
    private HttpServletRequest request;

    @Override
    public void update(long pBytesRead, long pContentLength, int pItems) {
        int percent = (int) (pBytesRead * 100.0 / pContentLength);
        int i = percent % 5;
        String uuid = request.getParameter("uuid");
        String fileName = request.getParameter("fileName");
        log.info("uuid：{}，fileName：{}，percent：{}", uuid, fileName, percent);
        if (StringUtils.isNotBlank(uuid) && StringUtils.isNotBlank(fileName) && i == 0) {
            updateProcess(percent, uuid, fileName);
        }
    }

    private void updateProcess(int percent, String uuid, String fileName) {
        dictService.saveOrUpdateDict(Dict.builder().dictType(DictTypeEnum.UPLOAD_PROCESS.getType()).dictKey(uuid).dictValue(percent + "").alias(fileName).build(), false);
    }

    public void fail(String comment) {
        String uuid = request.getParameter("uuid");
        if (StringUtils.isNotBlank(uuid)) {
            if (StringUtils.isBlank(comment)) {
                comment = "未知异常";
            }
            dictService.saveOrUpdateDict(Dict.builder().dictKey(uuid).comment(comment).build(), null);
        }
    }
}
