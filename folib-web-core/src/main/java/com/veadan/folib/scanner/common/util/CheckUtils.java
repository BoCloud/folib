package com.veadan.folib.scanner.common.util;

import com.alibaba.fastjson.JSONObject;

import java.io.*;

public class CheckUtils {

    //文件转JSON
    public static JSONObject jsonTransformFromFile(File file) {

        StringBuilder stringBuilder = new StringBuilder();
        try {
            FileInputStream fileInputStream = new FileInputStream(file);
            BufferedReader bf = new BufferedReader(new InputStreamReader(fileInputStream));
            String line;
            while ((line = bf.readLine()) != null) {
                stringBuilder.append(line);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return JSONObject.parseObject(stringBuilder.toString());
    }
}
