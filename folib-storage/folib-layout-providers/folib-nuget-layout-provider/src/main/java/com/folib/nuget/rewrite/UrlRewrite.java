package com.folib.nuget.rewrite;



public interface UrlRewrite {
    void rewrite(String v2BaseUrl);

    void rewrite(String v2BaseUrl, String packageId);
}
