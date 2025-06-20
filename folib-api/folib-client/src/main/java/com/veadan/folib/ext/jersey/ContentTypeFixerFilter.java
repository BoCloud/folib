package com.veadan.folib.ext.jersey;

import org.springframework.util.StringUtils;

import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientResponseContext;
import javax.ws.rs.client.ClientResponseFilter;
import java.io.IOException;

/**
 * @author leipenghui
 * @date 2025/2/11
 **/
public class ContentTypeFixerFilter implements ClientResponseFilter {

    @Override
    public void filter(ClientRequestContext clientRequestContext, ClientResponseContext clientResponseContext) throws IOException {
        try {
            // 获取响应中的 Content-Type
            String contentType = clientResponseContext.getHeaderString("Content-Type");
            String key = "charset=";
            if (StringUtils.hasText(contentType) && contentType.contains(key)) {
                // 分割 Content-Type 头部，获取类型和 charset 部分
                String[] parts = contentType.split(";");
                // 如果有 charset 部分，处理多个字符集
                if (parts.length > 1) {
                    // 获取 charset 部分，可能包含多个字符集
                    String charsetPart = parts[1].trim();
                    if (!StringUtils.hasText(charsetPart)) {
                        return;
                    }
                    if (!charsetPart.contains(",")) {
                        return;
                    }
                    // 如果 charset 部分有多个字符集，选择第一个字符集
                    String[] charsets = charsetPart.split(",");
                    // 选择第一个 charset
                    String selectedCharset = charsets[0].trim();
                    selectedCharset = selectedCharset.replace(key, "");
                    // 只保留一个字符集，修正 Content-Type
                    String fixedContentType = parts[0].trim() + "; charset=" + selectedCharset;
                    clientResponseContext.getHeaders().putSingle("Content-Type", fixedContentType);
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
