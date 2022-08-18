

package com.veadan.folib.scanner.common.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by Veadan on 2017/9/10.
 */
public class StringHelper {

    //判断是否为手机号
    public static boolean isMobileNO(String mobiles) {
        Pattern p = Pattern.compile("^((13[0-9])|(15[^4,\\D])|(18[0-9]))\\d{8}$");
        Matcher m = p.matcher(mobiles);
        return m.matches();
    }

    //判断邮箱是否为邮箱，查看是否合法
    public static boolean isEmail(String string) {
        if (string == null)
            return false;
        String regEx1 = "^([a-z0-9A-Z]+[-|\\.]?)+[a-z0-9A-Z]@([a-z0-9A-Z]+(-[a-z0-9A-Z]+)?\\.)+[a-zA-Z]{2,}$";
        Pattern p;
        Matcher m;
        p = Pattern.compile(regEx1);
        m = p.matcher(string);
        if (m.matches())
            return true;
        else
            return false;
    }


    public static String getObjectValue(Object obj){
        return obj==null?"":obj.toString();
    }
}
