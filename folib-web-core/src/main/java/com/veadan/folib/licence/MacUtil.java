package com.veadan.folib.licence;


import com.veadan.folib.components.node.NodeComponent;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

/**
 * @author veadan
 **/
@Slf4j
public class MacUtil {

    public static void main(String[] args) throws Exception {
        System.out.println("getMachineCode = " + getMachineCode());
    }

    /**
     * 获取机器码
     *
     * @return 机器码
     * @throws Exception 异常信息
     */
    public static String getMachineCode() throws Exception {
        Set<String> result = new HashSet<>();
        NodeComponent nodeComponent = SpringContextUtil.getBean(NodeComponent.class);
        String localHostId = nodeComponent.cassandraClusterInfo().getLocalHostId();
        log.debug("MachineCode localHostId [{}]", localHostId);
        result.add(localHostId);
        Properties props = System.getProperties();
        String javaVersion = props.getProperty("java.version");
        result.add(javaVersion);
        String javaVMVersion = props.getProperty("java.vm.version");
        result.add(javaVMVersion);
        String osVersion = props.getProperty("os.version");
        result.add(osVersion);
        String code = Encrpt.GetMD5Code(result.toString());
        return getSplitString(code, "-", 4);
    }


    /**
     * 分隔字符串
     *
     * @param str    源字符串
     * @param split  分隔字符
     * @param length 分隔长度
     * @return 分隔后的字符串
     */
    public static String getSplitString(String str, String split, int length) {
        int len = str.length();
        StringBuilder temp = new StringBuilder();
        for (int i = 0; i < len; i++) {
            if (i % length == 0 && i > 0) {
                temp.append(split);
            }
            temp.append(str.charAt(i));
        }
        String[] attrs = temp.toString().split(split);
        StringBuilder finalMachineCode = new StringBuilder();
        for (String attr : attrs) {
            if (attr.length() == length) {
                finalMachineCode.append(attr).append(split);
            }
        }
        return finalMachineCode.toString().substring(0,
                finalMachineCode.toString().length() - 1);
    }

}
