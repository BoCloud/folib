package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.scanner.common.util.file.FileFolibUtils;
import com.veadan.folib.services.JavaCmdService;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.annotation.PostConstruct;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;


@Service
@Slf4j
public class JavaCmdServiceImpl implements JavaCmdService {

    @Value("${cmdFile.path}")
    private String exeFilePath;

    @Value("${cmdFile.saveDir}")
    private String exeSaveDir;


    @PostConstruct
    public void init(){
        System.out.println(this.exeFilePath);
    }

    public  String getArtifactIndex(String format,String indexId,String chainId,String url) {
        // 关键是进去目录再执行
        try {
            // 调用CMD命令 这里确定是什么呢系统调用，todo 待拆分
            String command =  this.exeFilePath +" --format "+format+"  --indexId "+indexId+ " --chainId "+chainId+" --url "+url;
            Process process = Runtime.getRuntime().exec(command);
            // 获取命令输出结果
            InputStream inputStream = process.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, "GBK")); // 设置编码为GBK
            StringBuffer stringBuffer=new StringBuffer("");
            String line;
            while ((line = reader.readLine()) != null) {
                stringBuffer.append(line);
            }
            // 等待命令执行完成
            process.waitFor();
           return stringBuffer.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Map<String,String> parseFileAndDownLoad(CommonsMultipartFile file,String baseUrl)  {
        Map<String,String> result=new HashMap<String, String>() ;
        InputStream inputStream=null;
        try {
            inputStream=file.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, "GBK")); // 设置编码为GBK
            StringBuffer stringBuffer=new StringBuffer("");
            String line;
            while ((line = reader.readLine()) != null) {
                stringBuffer.append(line);
            }
            JSONArray jsonArray = JSON.parseArray(stringBuffer.toString());
            List<JSONObject> jsonObjectList = new ArrayList<>();
            for (int i = 0; i < jsonArray.size(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                jsonObjectList.add(jsonObject);
            }

            // 需要下载的url集合
            for (JSONObject itemData : jsonObjectList) {

                    String fileExtension = itemData.getString("fileExtension");
                    String groupId = itemData.getString("groupId").replaceAll("\\.", "/");
                    String artifactId = itemData.getString("artifactId");
                    String version = itemData.getString("version");
                    String artifactPath = String.format("%s/%s/%s/%s-%s.%s", groupId, artifactId, version, artifactId, version, fileExtension);
                    String url = baseUrl + artifactPath;
                try {
                    String[] dir = groupId.split("\\.");
                    String path = this.exeSaveDir;
                    for (int i = 0; i < dir.length; i++) {
                        if (!"".equals(dir[i])) {
                            path += File.separator + dir[i];
                        }
                    }
                    // 文件夹结构
                    path += File.separator + artifactId + File.separator + version;
                    File file1 = new File(path);
                    if (!file1.exists()) {
                        file1.mkdirs();
                    }
                    log.info("======》下载文件" + artifactPath);
                    FileFolibUtils.saveUrlAs(url, this.exeSaveDir, artifactPath, "GET");
                }catch (Exception exception){
                    // 下载失败的会记录下来
                    result.put(url,url);
                }
            }
        }catch (Exception e){
            log.error("===============>"+e.getStackTrace().toString());
        }finally {
            try {
                inputStream.close();
            }catch (Exception e){
                log.error("===============>"+e.getStackTrace().toString());
            }
        }
        return result;
    }


    /**
     *  判断当前系统类型
     */
    private String isLinux(){
        String osName = System.getProperty("os.name");
        if(osName.startsWith("Win")) {
            return "windows";
        }
        if(osName.startsWith("Linux")) {
            return "Linux";
        }
        return "Linux";
    }

}


