package com.veadan.folib.utils;

import com.tencentcloudapi.common.Credential;
import com.tencentcloudapi.common.profile.ClientProfile;
import com.tencentcloudapi.common.profile.HttpProfile;
import com.tencentcloudapi.tmt.v20180321.TmtClient;
import com.tencentcloudapi.tmt.v20180321.models.TextTranslateRequest;
import com.tencentcloudapi.tmt.v20180321.models.TextTranslateResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Translate {
    
    public static String translate(String content) {
        String result = "";
        try {
            Credential cred = new Credential("AKID52oNlyynofKm25vVyFLEuPipk9ML8iAQ", "62nMisoY4SYUEnpdS3RcZHm7GPL6h5f2");
            // 实例化一个http选项，可选的，没有特殊需求可以跳过
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("tmt.tencentcloudapi.com");
            // 实例化一个client选项，可选的，没有特殊需求可以跳过
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);
            // 实例化要请求产品的client对象,clientProfile是可选的
            TmtClient client = new TmtClient(cred, "ap-guangzhou", clientProfile);
            // 实例化一个请求对象,每个接口都会对应一个request对象
            TextTranslateRequest req = new TextTranslateRequest();
            // 英语
            req.setSourceText(content);
            req.setSource("en");
            req.setTarget("zh");
            // 设置ProjectId参数
            req.setProjectId(0L);
            // 返回的resp是一个TextTranslateResponse的实例，与请求对象对应
            TextTranslateResponse resp = client.TextTranslate(req);
            //返回中文
            result = resp.getTargetText();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        //返回中文
        return result;
    }

}
