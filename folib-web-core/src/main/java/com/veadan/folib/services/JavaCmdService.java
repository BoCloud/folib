package com.veadan.folib.services;

import org.springframework.web.multipart.commons.CommonsMultipartFile;

import java.io.InputStream;
import java.util.Map;

public interface JavaCmdService {

    String  getArtifactIndex(String format,String indexId,String chainId,String url);

    Map<String,String>  parseFileAndDownLoad(CommonsMultipartFile inputStream,String baseUrl);
}
