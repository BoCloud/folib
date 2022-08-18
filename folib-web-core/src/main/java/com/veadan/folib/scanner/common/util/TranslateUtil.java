/**
 * Copyright (c) blackbear, Inc All Rights Reserved.
 */
package com.veadan.folib.scanner.common.util;

import org.apache.commons.io.IOUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.io.InputStream;
import java.net.URLEncoder;
import java.text.MessageFormat;

/**
 * TranslateUtil
 * 
 * <pre>翻譯工具
 * PS: 透過google translate
 * </pre>
 * 
 * @author catty
 * @version 1.0, Created on 2011/9/2
 */
public class TranslateUtil {

	protected static final String URL_TEMPLATE = "http://translate.google.com/?langpair={0}&text={1}";
	protected static final String ID_RESULTBOX = "result_box";
	protected static final String ENCODING = "UTF-8";

	protected static final String AUTO = "auto"; // google自動判斷來源語系
	protected static final String TAIWAN = "zh-TW"; // 繁中
	protected static final String CHINA = "zh-CN"; // 簡中
	protected static final String ENGLISH = "en"; // 英
	protected static final String JAPAN = "ja"; // 日
	protected static final String KARTULI‎ = "ka";//格鲁吉亚
	protected static final String KOREAN = "ko";//朝鲜
	protected static final String RUSSIAN = "ru";//俄罗斯
	protected static final String ARABIC  = "ar";//阿拉伯
	protected static final String THAI  = "th";//泰国
	protected static final String ARAMAIC  = "ml";//泰国
	protected static final String TURKISH  = "tr";//土耳其

	/**
	 * <pre>Google翻譯
	 * PS: 交由google自動判斷來源語系
	 * </pre>
	 * 
	 * @param text
	 * @param target_lang 目標語系
	 * @return
	 * @throws Exception
	 */
	public static String translate(final String text, final String target_lang) throws Exception {
		return translate(text, AUTO, target_lang);
	}

	/**
	 * <pre>Google翻譯</pre>
	 * 
	 * @param text
	 * @param src_lang 來源語系
	 * @param target_lang 目標語系
	 * @return
	 * @throws Exception
	 */
	public static String translate(final String text, final String src_lang, final String target_lang)
			throws Exception {
		InputStream is = null;
		Document doc = null;
		Element ele = null;
		try {
			// create URL string
			String url = MessageFormat.format(URL_TEMPLATE,
					URLEncoder.encode(src_lang + "|" + target_lang, ENCODING),
					URLEncoder.encode(text, ENCODING));

			// connect & download html
			is = HttpClientUtil.downloadAsStream(url);

			// parse html by Jsoup
			doc = Jsoup.parse(is, ENCODING, "");
			ele = doc.getElementById(ID_RESULTBOX);
			String result = ele.text();
			return result;

		} finally {
			IOUtils.closeQuietly(is);
			is = null;
			doc = null;
			ele = null;
		}
	}
	/**
	 * 中文翻译------------->土耳其
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Turkish(final String text) throws Exception {
		return translate(text, CHINA, TURKISH);
	}
	
	/**
	 * 土尔其------------------->中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String turkish2china(final String text) throws Exception {
		return translate(text, TURKISH, CHINA);
	}

	/**
	 * <pre>Google翻譯: 簡中-->繁中</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String cn2tw(final String text) throws Exception {
		return translate(text, CHINA, TAIWAN);
	}

	/**
	 * <pre>Google翻譯: 繁中-->簡中</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String tw2cn(final String text) throws Exception {
		return translate(text, TAIWAN, CHINA);
	}

	/**
	 * <pre>Google翻譯: 英文-->繁中</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String en2tw(final String text) throws Exception {
		return translate(text, ENGLISH, TAIWAN);
	}

	/**
	 * <pre>Google翻譯: 繁中-->英文</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String tw2en(final String text) throws Exception {
		return translate(text, TAIWAN, ENGLISH);
	}

	/**
	 * <pre>Google翻譯: 日文-->繁中</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String jp2tw(final String text) throws Exception {
		return translate(text, JAPAN, TAIWAN);
	}

	/**
	 * <pre>Google翻譯: 繁中-->日</pre>
	 * 
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String tw2jp(final String text) throws Exception {
		return translate(text, TAIWAN, JAPAN);
	}
	/**
	 * 简体中文--------------->日文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2jp(final String text) throws Exception {
		return translate(text, CHINA, JAPAN);
	}
	
	public static String jp2china(final String text) throws Exception {
		return translate(text, JAPAN, CHINA);
	}
	
	
	/**
	 * 中文---------->格鲁吉亚
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Kartuli‎(final String text) throws Exception {
		return translate(text, CHINA, KARTULI‎);
	}
	
	/**
	 * 格鲁吉亚---------->中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String Kartul2china‎(final String text) throws Exception {
		return translate(text, KARTULI‎, CHINA);
	}
	
	/**
	 * 朝鲜------------>中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String korean2china‎(final String text) throws Exception {
		return translate(text, KOREAN, CHINA);
	}
	
	
	/**
	 * 中文------------>朝鲜
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2korean‎(final String text) throws Exception {
		return translate(text, CHINA, KOREAN);
	}
	
	/**
	 * 中文-------------->俄罗斯
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Russian(final String text)throws Exception{
		return translate(text, CHINA, RUSSIAN);
	}
	
	/**
	 * 俄罗斯文------------>中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String russian2China(final String text)throws Exception{
		return translate(text, RUSSIAN, CHINA);
	}
	
	/**
	 * 中文言---------->阿拉伯
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Arabic(final String text)throws Exception{
		return translate(text, CHINA, ARABIC);
	}
	/**
	 * 阿拉伯---------------->中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String arabic2China(final String text)throws Exception{
		return translate(text, ARABIC, CHINA);
	}
	/**
	 * 中文------------>泰国
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Thai(final String text)throws Exception{
		return translate(text, CHINA, THAI);
	}
	
	/**
	 * 泰国------------->中国
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String thai2China(final String text)throws Exception{
		return translate(text, THAI, CHINA);
	}
	
	/**
	 * 中文----------》马拉亚拉姆语
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String china2Aramaic(final String text)throws Exception{
		return translate(text, CHINA, ARAMAIC);
	}
	/**
	 * 马拉亚拉姆语------------>中文
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public static String aramaic2China(final String text)throws Exception{
		return translate(text, ARAMAIC, CHINA);
	}

	public static void main(String[] args) throws Exception {
		String tx =  TranslateUtil.china2korean‎("你好！");
		String test = "深圳市人民政府";
		if(test.trim() == "" || test.trim().length() == 0){
			System.out.println("输入不能为空!");
		}
		String str = TranslateUtil.translate(test, TranslateUtil.AUTO,TranslateUtil.KOREAN);
//		 str = str.replace(" ", "");
		System.out.println(tx);
	}

}