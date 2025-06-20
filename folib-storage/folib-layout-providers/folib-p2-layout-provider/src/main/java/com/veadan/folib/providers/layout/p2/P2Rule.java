package com.veadan.folib.providers.layout.p2;


import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class P2Rule
{

	private String output;

	private String filter;

	@XmlAttribute
	public String getOutput() {
		return output;
	}

	public void setOutput(String output) {
		this.output = output;
	}

	@XmlAttribute
	public String getFilter() {
		return filter;
	}

	public void setFilter(String filter) {
		this.filter = filter;
	}
}
