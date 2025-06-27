//package com.veadan.folib.listener;
//
//import com.veadan.folib.entity.Dict;
//import com.veadan.folib.enums.DictTypeEnum;
//import com.veadan.folib.services.DictService;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.stereotype.Component;
//import org.springframework.web.context.annotation.RequestScope;
//import jakarta.servlet.http.HttpServletRequest;
//
//import org.apache.commons.fileupload2.core.ProgressListener;
//
//
//
///**
// * @author veadan
// * @date 2023/2/12
// **/
//
//@Slf4j
//@Component
//@RequestScope
//public class FileUploadProgressListener implements ProgressListener {
//
//    @Autowired
//    private DictService dictService;
//
//    @Autowired
//    private HttpServletRequest request;
//
//    @Override
//    public void update(long bytesRead, long contentLength, int items) {
//        int percent = (int) (bytesRead * 100.0 / contentLength);
//        if (percent % 5 == 0) {
//            String uuid = request.getParameter("uuid");
//            String fileName = request.getParameter("fileName");
//            if (StringUtils.isNoneBlank(uuid, fileName)) {
//                log.debug("Progress: uuid={}, file={}, {}%", uuid, fileName, percent);
//                updateProcess(percent, uuid, fileName);
//            }
//        }
//    }
//
//    public void fail(String message) {
//        String uuid = request.getParameter("uuid");
//        if (StringUtils.isNotBlank(uuid)) {
//            dictService.saveOrUpdateDict(
//                    Dict.builder()
//                            .dictKey(uuid)
//                            .comment(StringUtils.defaultIfBlank(message, "未知错误"))
//                            .build(),
//                    null
//            );
//        }
//    }
//
//    private void updateProcess(int percent, String uuid, String fileName) {
//        dictService.saveOrUpdateDict(
//                Dict.builder()
//                        .dictType(DictTypeEnum.UPLOAD_PROCESS.getType())
//                        .dictKey(uuid)
//                        .dictValue(String.valueOf(percent))
//                        .alias(fileName)
//                        .build(),
//                false
//        );
//    }
//}
