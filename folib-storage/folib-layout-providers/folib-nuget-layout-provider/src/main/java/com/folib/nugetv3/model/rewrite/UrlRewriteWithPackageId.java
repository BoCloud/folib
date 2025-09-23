package com.folib.nugetv3.model.rewrite;


public interface UrlRewriteWithPackageId {
    /**
     * 重写注册信息
     *
     * @param v3RegistrationBaseUrl V3注册信息的基础URL
     * @param v2BaseUrl              V2包内容的基础URL
     */
    void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl, String packageId);
}
