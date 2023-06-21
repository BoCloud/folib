package com.veadan.folib.services;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.http.HttpRequest;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.licence.ActivateVo;
import com.veadan.folib.licence.MacUtil;
import com.veadan.folib.storage.Storage;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;


@Component
public class CodeActivateService {

    private static final String SERVER_URL = "http://license.folib.com/";

    @Inject
    private  ConfigurationManagementService configurationManagementService;

    @Value("${folib.home}")
    private String homePath;

    public static void main(String[] args) {
        String md5=SecureUtil.md5("datatoJSONString()"+"folib!@#$%^&*ABCD");
        System.out.printf(md5);
    }

    public JSONObject activate(String code,boolean isPoc) throws Exception {
        JSONObject data = new JSONObject();
        if(!isPoc){
            try {
                String result = HttpRequest.get(SERVER_URL)
                        .header("Content-Type", "application/json")
                        .form("activate",code)
                        .form("machineCode", MacUtil.getMachineCode())
                        .execute().body();
                JSONObject res = JSON.parseObject(result);
                if (res.getBoolean("rel")) {
                    //查看激活
                    data = res.getJSONObject("data");
                    data.put("rel", true);
                    String md5=SecureUtil.md5(data.toJSONString()+"folib!@#$%^&*ABCD");
                    data.put("md5",md5);

                    File file = new File(homePath + "/etc/lic/folib.lic");
                    //文件是否存在
                    if (file.exists()) {
                        FileUtil.writeUtf8String(data.toJSONString(), file);
                    } else {
                        FileUtil.touch(file);
                        FileUtil.writeUtf8String(data.toJSONString(), file);
                    }
                } else {
                    data = res;
                }
            } catch (Exception exception) {
                data.put("rel", false);
                data.put("message", "激活失败，网络异常");
                System.out.printf(exception.getMessage());
            }
        }else {
            final List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                    .getStorages()
                    .values());

            if(storages.size()<=2) {
                data.put("activate", "已激活");
                //试用一个月
                data.put("endDate", DateUtil.nextMonth().toDateStr());
                data.put("codes", code);
                data.put("type", "试用版");
                data.put("mac", MacUtil.getMachineCode());
                data.put("rel", true);
                String md5 = SecureUtil.md5(data.toJSONString() + "folib!@#$%^&*ABCD");
                data.put("md5", md5);
                File file = new File(homePath + "/etc/lic/folib.lic");
                //文件是否存在
                if (file.exists()) {
                    FileUtil.writeUtf8String(data.toJSONString(), file);
                } else {
                    FileUtil.touch(file);
                    FileUtil.writeUtf8String(data.toJSONString(), file);
                }
            }else {
                data.put("rel", false);
                data.put("message", "激活失败，你已经试用过一次了，不可重复，请换一台机器");
            }
        }
        return data;
    }


    public ActivateVo isNotActivate() throws Exception {
        JSONObject dataObj;
        ActivateVo activateVo = new ActivateVo();
        File file = new File(homePath + "/etc/lic/folib.lic");
        //文件是否存在
        if (file.exists()&&FileUtil.isNotEmpty(file)) {
            String dataStr= FileUtil.readUtf8String(file);
            try {
                dataObj=JSONObject.parseObject(dataStr);
                String md5 = dataObj.getString("md5");
                dataObj.remove("md5");
                String md51=SecureUtil.md5(dataObj.toJSONString()+"folib!@#$%^&*ABCD");
                if(md51.equals(md5)){
                    //机器码经过校验之后范围为mac字段
                    if( dataObj.getString("mac").equals(MacUtil.getMachineCode())){
                        activateVo.setHaveError(false);
                        String endTime = dataObj.getString("endDate");

                        SimpleDateFormat sdf=new SimpleDateFormat("yyyy-MM-dd");
                        Date bt=new Date();
                        Date et=sdf.parse(endTime);
                        if(bt.compareTo(et)<=0){
                            activateVo.setDalyOut(false);
                            activateVo.setObject(dataObj);
                        }else {
                            activateVo.setDalyOut(true);
                        }
                    }else {
                        activateVo.setHaveError(true);
                    }
                }else {
                    activateVo.setHaveError(true);
                }


            }catch (Exception exception){
                activateVo.setHaveError(true);
                activateVo.setDalyOut(true);
            }
        } else {
            activateVo.setHaveError(true);
            activateVo.setDalyOut(true);
        }
        activateVo.setMac(MacUtil.getMachineCode());

        return activateVo;
    }

}
