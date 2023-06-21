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
//    public static void main(String [] args) {
//        try{
//            // 实例化一个认证对象，入参需要传入腾讯云账户 SecretId 和 SecretKey，此处还需注意密钥对的保密
//            // 代码泄露可能会导致 SecretId 和 SecretKey 泄露，并威胁账号下所有资源的安全性。以下代码示例仅供参考，建议采用更安全的方式来使用密钥，请参见：https://cloud.tencent.com/document/product/1278/85305
//            // 密钥可前往官网控制台 https://console.cloud.tencent.com/cam/capi 进行获取
//
//            //这是徐新平的付费账号
//            Credential cred = new Credential("AKID52oNlyynofKm25vVyFLEuPipk9ML8iAQ", "62nMisoY4SYUEnpdS3RcZHm7GPL6h5f2");
//            // 实例化一个http选项，可选的，没有特殊需求可以跳过
//            HttpProfile httpProfile = new HttpProfile();
//            httpProfile.setEndpoint("tmt.tencentcloudapi.com");
//            // 实例化一个client选项，可选的，没有特殊需求可以跳过
//            ClientProfile clientProfile = new ClientProfile();
//            clientProfile.setHttpProfile(httpProfile);
//
////            clientProfile.setRegion("ap-guangzhou");
//            // 实例化要请求产品的client对象,clientProfile是可选的
//            TmtClient client = new TmtClient(cred, "ap-guangzhou", clientProfile);
//            // 实例化一个请求对象,每个接口都会对应一个request对象
//            TextTranslateRequest req = new TextTranslateRequest();
//// 英语
//            req.setSourceText("Jenkins AbsInt a³ Plugin 1.1.0 and earlier does not configure its XML parser to prevent XML external entity (XXE) attacks.");
//
//            req.setSource("en");
//            req.setTarget("zh");
//            req.setProjectId(0);  // 设置ProjectId参数
//
//            // 返回的resp是一个TextTranslateResponse的实例，与请求对象对应
//            TextTranslateResponse resp = client.TextTranslate(req);
//            //返回中文
//            System.out.printf(resp.getTargetText());
//            // 输出json格式的字符串回包
////            System.out.println(TextTranslateResponse.toJsonString(resp));
//
//        } catch (TencentCloudSDKException e) {
//            System.out.println(e.toString());
//        }
//    }

    public static String translate(String content) {
        String result = "";
        try {
            // 实例化一个认证对象，入参需要传入腾讯云账户 SecretId 和 SecretKey，此处还需注意密钥对的保密
//            // 代码泄露可能会导致 SecretId 和 SecretKey 泄露，并威胁账号下所有资源的安全性。以下代码示例仅供参考，建议采用更安全的方式来使用密钥，请参见：https://cloud.tencent.com/document/product/1278/85305
//            // 密钥可前往官网控制台 https://console.cloud.tencent.com/cam/capi 进行获取

            //这是徐新平的付费账号
            Credential cred = new Credential("AKID52oNlyynofKm25vVyFLEuPipk9ML8iAQ", "62nMisoY4SYUEnpdS3RcZHm7GPL6h5f2");
            // 实例化一个http选项，可选的，没有特殊需求可以跳过
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("tmt.tencentcloudapi.com");
            // 实例化一个client选项，可选的，没有特殊需求可以跳过
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);

//            clientProfile.setRegion("ap-guangzhou");
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
