<template>
  <div id="settings">
    <a-tabs class="tabs-sliding" default-active-key="1" @change="tabChange($event)">
      <a-tab-pane key="1" tab="全局配置">
        <a-row type="flex" :gutter="[24,24]">
          <a-col :span="24" :lg="6">
            <!-- Page Anchors -->
            <a-affix :offset-top=" navbarFixed ? 100 : 10 ">
              <a-card :bordered="false" class="header-solid mb-24">
                <a-anchor :targetOffset=" navbarFixed ? 100 : 10 " :affix="false" @click="handleClick">
                  <a-anchor-link href="#basic">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="snippets" theme="filled" class="text-gray-6 text-lg"/>
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">基础信息配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#smtp">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="unlock" theme="filled" class="text-gray-6 text-lg"/>
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">SMTP配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#proxy">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="safety-certificate" theme="filled" class="text-gray-6 text-lg"/>
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">网络代理配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#cors">
                    <div slot="title" class="ant-list-item-meta">
                      <a-icon type="dashboard" theme="filled" class="text-gray-6 text-lg"/>
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">CORS配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                </a-anchor>
              </a-card>
            </a-affix>
            <!-- / Page Anchors -->

          </a-col>
          <a-col :span="24" :lg="18">
            <!-- Basic Info card -->
            <a-card :bordered="false" id="basic" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">基础 配置</h5>
              </template>
              <a-form
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" label="应用名称" :colon="false">
                      <a-input placeholder="folib" v-model="serverSettings.instanceName"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" label="Base Url" :colon="false">
                      <a-input placeholder="http://localhot:38080" v-model="serverSettings.baseUrl"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" label="端口号" :colon="false">
                      <a-input placeholder="38080" v-model="serverSettings.port"/>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>应用名称修改会自动修改到配置文件</li>
                  <li>baseurl,如果你使用了反向代理公网等情况下可以使用它</li>
                  <li>foli-server服务的后端通信端口</li>
                </ul>
              </a-form>
            </a-card>
            <!-- / Basic Info card -->

            <!-- Change Password card -->
            <a-card :bordered="false" id="smtp" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">SMTP配置</h5>
              </template>
              <a-form
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="用户名" :colon="false">
                      <a-input placeholder="SMTP用户名" v-model="serverSettings.smtpConfigurationForm.username"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="密码" :colon="false">
                      <a-input placeholder="SMTP密码" v-model="serverSettings.smtpConfigurationForm.password"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="HOST" :colon="false">
                      <a-input placeholder="HOST" v-model="serverSettings.smtpConfigurationForm.host"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" label="端口" :colon="false">
                      <a-input placeholder="端口" v-model="serverSettings.smtpConfigurationForm.port"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" label="协议类型" :colon="false">
                      <a-select
                          v-model="serverSettings.smtpConfigurationForm.connection"
                          show-search
                          placeholder="协议选择"
                          option-filter-prop="children"
                          :filter-option="filterOption"
                      >
                        <a-select-option value="None">
                          None
                        </a-select-option>
                        <a-select-option value="Plain">
                          Plain
                        </a-select-option>
                        <a-select-option value="SSL">
                          SSL
                        </a-select-option>
                        <a-select-option value="STL">
                          STL
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>该配置是用来设置系统邮件</li>
                  <li>程序中某些事件会对相关用户进行邮件通知</li>
                  <li>该功能未来开放</li>
                </ul>
              </a-form>
            </a-card>


            <!-- Two-factor authentication card -->
            <a-card :bordered="false" id="proxy" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">网络代理</h5>
              </template>
              <a-form
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="用户名" :colon="false">
                      <a-input placeholder="代理用户名" v-model="serverSettings.proxyConfigurationForm.username"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="密码" :colon="false">
                      <a-input placeholder="代理密码" v-model="serverSettings.proxyConfigurationForm.password"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="12">
                    <a-form-item class="mb-10" label="代理地址" :colon="false">
                      <a-input placeholder="代理地址" v-model="serverSettings.proxyConfigurationForm.host"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" label="端口" :colon="false">
                      <a-input placeholder="端口" v-model="serverSettings.proxyConfigurationForm.port"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="6">
                    <a-form-item class="mb-10" label="类型" :colon="false">
                      <a-select
                          v-model="serverSettings.proxyConfigurationForm.type"
                          show-search
                          placeholder="选择"
                          option-filter-prop="children"
                          :filter-option="filterOption"
                      >
                        <a-select-option value="">
                          None
                        </a-select-option>
                        <a-select-option value="HTTP">
                          HTTP
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>该配置是用来针对某些情况下网络无法通信的情况下</li>
                  <li>例如需要配置代理后可以访问公网</li>
                  <li>可以通过代理获取公网仓库的依赖</li>
                </ul>
              </a-form>
            </a-card>

            <a-card :bordered="false" id="cors" class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">CORS配置</h5>
              </template>
              <a-form
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="24" :lg="16">
                    <a-form-item class="tags-field mb-10" label="Origins" :colon="false">
                      <a-select mode="tags" :defaultValue="serverSettings.corsConfigurationForm.allowedOrigins[0]" style="width: 100%"
                                placeholder="例如：*">
                        <a-select-option v-for="(tag,index) in serverSettings.corsConfigurationForm.allowedOrigins" :key="index" :value="tag">
                          {{ tag }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24" :lg="8">
                    <a-form-item class="mb-10" label="开启所有" :colon="false">
                      <span class="mr-15">开启</span>
                      <a-switch v-model="serverSettings.corsConfigurationForm.corsAllowAll"/>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>开启所有意味着不再有跨域限制</li>
                </ul>
              </a-form>
            </a-card>
            <a-card :bordered="false" id="delete-account" class="header-solid mb-24">
              <a-form
                  id="components-form-demo-normal-login"
                  class="login-form list-settings-sessions"
                  :hideRequiredMark="true"
              >
                <a-row type="flex" align="middle">
                  <a-col style="min-width: 40px;" class="text-center">
                    <!--										<a-switch></a-switch>-->
                  </a-col>
                  <a-col class="pl-15">
                    <p class="mb-0 font-semibold">保存操作</p>
                    <small class="text-dark">该保存按钮将会针对以上4个部分的修改统一保存</small>
                  </a-col>
                  <a-col :span="24" :md="12" class="ml-auto"
                         style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button @click="getServerSettings">
                      取 消
                    </a-button>
                    <a-button type="danger" class="ml-10" @click="saveServerSettings">
                      保 存
                    </a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </a-col>
        </a-row>
      </a-tab-pane>
      <a-tab-pane key="2" tab="安全策略">
        <div class="header-solid"
                style="height:60vh;display:flex;alignItems:center;justifyContent:center"> 
          <a-card class="white-card">
            <template #title>
              <p>白名单</p>
            </template>
            <div class="o-btn" @click="() => (showVulnerabilitiesModal = true,vulnerabilitiesType=1)">
              <img src="images/folib/white.svg"/>
            </div>  
            <div class="white-group">
              <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.whiteList" :pagination="{pageSize: 5,total:vulnerabilities.whiteList.length,showLessItems:true}">
                <a-list-item slot="renderItem" key="index" slot-scope="item, index">
                  <label>{{item}}</label>
                  <template #extra>
                    <a-popconfirm
                          title="确定要从白名单移除吗？"
                          ok-text="确定"
                          cancel-text="取消"
                          class="d-popconfirm"
                          @confirm="removeWhite(item)"
                        >
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd" d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z" fill="#111827"/>
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-popconfirm>
                  </template>
                </a-list-item>
              </a-list>
            </div>
          </a-card>
          <a-card class="black-card">
            <template #title>
              <p>黑名单</p>
            </template>
            <div class="o-btn o-black" @click="() => (showVulnerabilitiesModal = true,vulnerabilitiesType=2)">
              <img src="images/folib/black.svg"/>
            </div>   
            <div class="black-group">
              <a-list item-layout="vertical" size="large" :data-source="vulnerabilities.blackList" :pagination="{pageSize: 5,total:vulnerabilities.blackList.length,showLessItems:true}">
                <a-list-item slot="renderItem" key="index" slot-scope="item, index">
                  {{item}}
                  <template #extra>
                    <a-popconfirm
                          title="确定要从黑名单移除吗？"
                          ok-text="确定"
                          cancel-text="取消"
                          class="d-popconfirm"
                          @confirm="removeBlack(item)"
                        >
                      <svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd" d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z" fill="#111827"/>
                      </svg>
                      <span class="text-danger">DELETE</span>
                    </a-popconfirm>
                  </template>
                </a-list-item>
              </a-list>
            </div>
          </a-card>
        </div>
      </a-tab-pane>
      <a-tab-pane key="3" tab="LDAP配置">
        <div class="mx-auto mt-50" style="max-width: 1000px;">
          <div class="mb-50" style="max-width: 1000px;">

            <a-steps progress-dot v-model="step">
              <a-step title="连接配置"/>
              <a-step title="用户映射"/>
              <a-step title="角色映射"/>
            </a-steps>
          </div>

          <div class="mb-24">
            <!-- Step 1 -->
            <a-card v-if="step == 0" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }"
                    :headStyle="{paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">连接配置</h5>
                <p class="font-regular">该部分配置用于和LDAP建立连接</p>
              </template>
              <a-form
                  @submit="handleSubmit"
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-form-item class="mb-10" label="URL" :colon="false">
                      <a-input placeholder="例如: ldap://1.2.3.4/dc=domain,dc=com" v-model="ldap.url"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="绑定DN" :colon="false">
                      <a-input placeholder="例如:cn=manager,ou=users,dc=domain,dc=com" v-model="ldap.managerDn"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="绑定密码" :colon="false">
                      <a-input placeholder="********" v-model="ldap.managerPassword"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="是否开启LDAP服务" :colon="false">
                      <span class="mr-15">{{ ldap.enableProvider ? '开启' : '关闭' }}</span>
                      <a-switch default-checked v-model="ldap.enableProvider"/>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary" @click="moveStep(1)" class="px-25">下一步</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 2 -->
            <a-card v-else-if="step == 1" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }"
                    :headStyle="{paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">用户映射</h5>
              </template>
              <a-form
                  @submit="handleSubmit"
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="用户搜索对象" :colon="false">
                      <a-input placeholder="例如：ou=Users" v-model="ldap.userSearchBase"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="用户过滤条件" :colon="false">
                      <a-input placeholder="例如：(uid={0})" v-model="ldap.userSearchFilter"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="16">
                    <a-form-item class="tags-field mb-10" label="在验证查找用户时将使用以下用户DN列表" :colon="false">
                      <a-select mode="tags" :defaultValue="ldap.userDnPatternList" style="width: 100%"
                                placeholder="例如：uid={0},uid={0},ou=Admins">
                        <a-select-option v-for="(tag,index) in ldap.userDnPatternList" :key="index" :value="tag">
                          {{ tag }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10" label="用户密码是否是Base64加密?" :colon="false">
                      <span class="mr-15">{{ ldap.userPasswordEncoded ? '是' : '否' }}</span>
                      <a-switch default-checked v-model="ldap.userPasswordEncoded"/>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)" class="px-25">上一步</a-button>
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary" @click="moveStep(1)" class="px-25">下一步</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 3 -->
            <a-card v-else-if="step == 2" :bordered="false" class="header-solid" :bodyStyle="{paddingTop: 0 }"
                    :headStyle="{paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">角色匹配</h5>
              </template>
              <a-form
                  @submit="handleSubmit"
                  :hideRequiredMark="true"
              >
                <a-row :gutter="[24]">
                  <a-col :span="8">
                    <a-form-item class="mb-10" label="Group匹配" :colon="false">
                      <a-input placeholder="例如：ou=Groups" v-model="ldap.authorities.groupSearchBase"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10" label="Group过滤条件" :colon="false">
                      <a-input placeholder="例如：(uniqueMember={0})" v-model="ldap.authorities.groupSearchFilter"/>
                    </a-form-item>
                  </a-col>
                  <a-col :span="6">
                    <a-form-item class="mb-10" label="组织单元" :colon="false">
                      <a-input placeholder="例如：cn,ou" v-model="ldap.authorities.groupRoleAttribute"/>
                    </a-form-item>

                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]" v-for="(item,index) in ldap.roleMappingList" :key="index">
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="FOLIB角色" :colon="false">
                      <a-select v-model="item.folibRole">
                        <a-select-option v-for="(i,index) in assignableRoles" :key="index" :value="i.name">
                          {{ i.name }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10" label="LDAP角色" :colon="false">
                      <a-col :span="20">
                        <a-input placeholder="输入LDAP的角色" v-model="item.externalRole"/>
                      </a-col>
                      <a-col :span="4">
                        <a-button type="link" size="small" @click="roleMappingDelHandle(item,index)">
                          <svg width="16" height="16" viewBox="0 0 20 20" fill="none"
                               xmlns="http://www.w3.org/2000/svg">
                            <path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
                                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                  fill="#111827"/>
                          </svg>
                        </a-button>
                      </a-col>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-button type="dashed" style="width: 91.66666%" @click="roleMappingAddHandle">
                      <a-icon type="plus"/>
                      添加
                    </a-button>
                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)" class="px-25">上一步</a-button>
                  </a-col>
                  <a-col :span="12" class="text-right">
                    <a-button type="primary"  class="px-25" @click="putLdap">完成</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </div>
        </div>
      </a-tab-pane>
      <a-tab-pane key="4" tab="节点许可配置">
        <a-row type="flex" :gutter="24">

          <!-- 许可信息-->
          <a-col :span="24" :md="12" class="mb-24">

            <a-card :bordered="false" class="header-solid h-full card-profile-information" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }" :headStyle="{paddingRight: 0,}">
              <template #title>
                <h6 class="font-semibold m-0">FOLIB 许可信息</h6>
              </template>
              <a-button v-if="machineInfo.haveError||machineInfo.dalyOut" type="link" slot="extra" @click="copy(machineInfo.mac)">
                <a-icon type="copy" theme="twoTone" />
              </a-button>
              <p class="text-dark" v-if="!machineInfo.haveError">
                尊敬的用户,很荣幸您选择使用FO Library!
                在接下来FO Library将会为您提供统一软件包管理。
                IT数字化转型道路长远,FO Library与您随行！
              </p>
              <p class="text-dark" v-if="machineInfo.haveError&&machineInfo.dalyOut">
                尊敬的用户,很荣幸您选择试用FO Library!如果觉得符合您企业信创发展战略,可选择购买正式版本。我们的销售热线：400888888888
              </p>
              <p class="text-dark" v-if="(!machineInfo.haveError)&&machineInfo.dalyOut">
                尊敬的用户,很荣幸您选择试用FO Library!您的序列号已经过期，为了更好的为您提供服务请尽快续期。我们的销售热线：400888888888
              </p>
              <hr class="my-25">
              <a-descriptions :title="machineInfo.haveError?'未激活':(!machineInfo.haveError)&&machineInfo.dalyOut?'已过期':'已激活'" :column="1">
                <a-descriptions-item label="机器码">
                 {{machineInfo.mac}}
                </a-descriptions-item>
                <a-descriptions-item label="版本类型">
                  {{machineInfo.haveError?"无":machineInfo.object.type}}
                </a-descriptions-item>
                <a-descriptions-item label="有效日期">
                  {{machineInfo.haveError?"无":machineInfo.object.endDate}}
                </a-descriptions-item>
                <a-descriptions-item label="序列号">
                  {{machineInfo.haveError?"无":machineInfo.object.codes}}
                </a-descriptions-item>
                <a-descriptions-item label="是否激活">
                  <a href="http://folib.com" class="mx-5 px-5" v-if="!machineInfo.haveError">
                    <a-avatar :size="24" shape="square" src="images/folib/isactivate.svg" />
                  </a>
                  <a href="http://folib.com" class="mx-5 px-5" v-if="machineInfo.haveError">
                    <a-avatar :size="24" shape="square" src="images/folib/notactivate.svg" />
                  </a>
                </a-descriptions-item>
              </a-descriptions>
            </a-card>

          </a-col>
          <a-col :span="24" :md="12" class="mb-24">
            <a-card :bordered="false" class="header-solid h-full card-profile-information" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }" :headStyle="{paddingRight: 0,}">
              <template #title>
                <h6 class="font-semibold m-0">激活序列号</h6>
              </template>
              <a-button type="link" slot="extra" @click="postActivate(false)">
                <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted" d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z" fill="#111827"/>
                  <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827"/>
                </svg>
                正式激活
              </a-button>
              <a-button type="link" slot="extra" @click="postActivate(true)">
                <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted" d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z" fill="#111827"/>
                  <path class="fill-muted" d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z" fill="#111827"/>
                </svg>
                试用
              </a-button>
              <p class="$color-muted">
                你可以复制左侧机器码来获取FOLIB的序列号，并将序列号填入下方。正式激活请确保激活过程可以联网，如果需要开通网络策略，请将"license.folib.com"设置为白名单
              </p>
              <hr class="my-25">
              <a-form-item class="mb-10" label="序列号" :colon="false">
                <a-textarea :rows="8" placeholder="请粘贴本机器码的序列号" v-model="activateCode" />
              </a-form-item>
            </a-card>
            <!-- / Conversations Card -->

          </a-col>


        </a-row>
      </a-tab-pane>
    </a-tabs>
    <a-modal
      v-model="showVulnerabilitiesModal"
      :title="vulnerabilitiesType===1?'添加白名单':'添加黑名单'"
      :maskClosable="false"
      cancelText="取消"
      okText="确定"
      @cancel="vulnerabilitiesModalCancel()"
      @ok="addVulnerabilities()"
      centered
    >
      <a-input v-model="uuid" placeholder="请输入漏洞编号" />
    </a-modal>
  </div>

</template>

<script>

import {getServerSettings, postServerSettings, getLdap, putLdap, getMachineCode,postActivate,checkMachineCode} from "@/api/settings";
import {getUsersCreateFields} from "@/api/users";
import {getVulnerabilities,addVulnerabilitiesWhite,addVulnerabilitiesBlack,removeVulnerabilitiesWhite,removeVulnerabilitiesBlack} from "@/api/folib";



export default {
  props: ['navbarFixed'],
  data() {
    return {
      step: 0,
      serverSettings: {
        instanceName: "folib",
        baseUrl: "http://localhost:38080/",
        port: 38080,
        corsConfigurationForm: {allowedOrigins: ["*"], corsAllowAll: false},
        smtpConfigurationForm: {host: null, port: null, username: null, password: null, connection: null},
        proxyConfigurationForm: {
          host: null,
          port: null,
          type: null,
          username: null,
          password: null,
          nonProxyHosts: []
        }
      },
      assignableRoles: [],
      ldap: {
        url: "ldap://127.0.0.1:53389/dc=carlspring,dc=com",
        managerDn: "cn=admin,dc=carlspring,dc=com",
        managerPassword: "password",
        userPasswordEncoded: false,
        userSearchBase: "ou=Users",
        userSearchFilter: "(uid={0})",
        roleMappingList: [{externalRole: "Admins", folibRole: "ADMIN"}],
        userDnPatternList: ["uid={0},ou=Users"],
        enableProvider: false,
        authorities: {
          groupSearchBase: "ou=Groups",
          groupSearchFilter: "(uniqueMember={0})",
          searchSubtree: true,
          groupRoleAttribute: "cn",
          rolePrefix: "",
          convertToUpperCase: false
        }
      },
      machineInfo:{mac:null,haveError:true,dalyOut:true,object:null},
      activateCode:null,
      vulnerabilities: {
        whiteList: [],
        blackList: [],
      },
      showVulnerabilitiesModal: false,
      vulnerabilitiesType: null,
      uuid: "",
    };
  },
  computed: {},
  created() {
    this.getServerSettings()
    this.getLdap()
    this.getUsersCreateFields()
    this.getMachineCode()
    this.getVulnerabilities()
  },
  methods: {
    handleSubmit(e) {
      e.preventDefault();
      this.form.validateFields((err, values) => {
        if (!err) {
        }
      });
    },
    tabChange(key) {
      if(key === '2'){
        this.getVulnerabilities()
      }
    },
    moveStep(distance) {
      this.step += distance;
    },
    getServerSettings() {
      getServerSettings().then(res => {
        this.serverSettings = res
      })
    },
    saveServerSettings() {
      postServerSettings(this.serverSettings).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: '保存成功',
          })
        }, 100)
      })
    },
    handleClick(e, link) {
      e.preventDefault();
    },
    // Languages select field search method.
    filterOption(input, option) {
      return (
          option.componentOptions.children[0].text.toLowerCase().indexOf(input.toLowerCase()) >= 0
      );
    },
    getLdap() {
      getLdap().then(res => {
        this.ldap = res
      })
    },
    putLdap(){
      putLdap(this.ldap).then(res=>{
        setTimeout(() => {
          this.$notification.success({
            message: '保存成功',
          })
        }, 100)
      })
    },
    getUsersCreateFields() {
      getUsersCreateFields().then(res => {
        let roles = res.formDataValues[0].values
        this.assignableRoles = roles
      })
    },
    roleMappingDelHandle(item, index) {
      this.ldap.roleMappingList.splice(index, 1)
    },
    roleMappingAddHandle() {
      this.ldap.roleMappingList.splice(this.ldap.roleMappingList.length, 0, {externalRole: null, folibRole: null})
    },
    getMachineCode(){
      checkMachineCode().then(res=>{
      this.machineInfo=res
     })
    },
    copy(code) {
      var input = document.createElement("input"); // 创建input对象
      input.value = code; // 设置复制内容
      document.body.appendChild(input); // 添加临时实例
      input.select(); // 选择实例内容
      document.execCommand("Copy"); // 执行复制
      document.body.removeChild(input); // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: '复制成功'
        })
      }, 100)
    },
    postActivate(isPoc){
      if(this.activateCode){
        postActivate(this.activateCode,isPoc).then(res=>{
          if(res.rel){
            setTimeout(() => {
              this.$notification.success({
                message: '激活成功'
              })
              this.getMachineCode()
            }, 100)
          }else {
            setTimeout(() => {
            this.$notification.open({
              class: 'ant-notification-warning',
              message: '激活失败',
              description: res.message,
            });
            }, 100)
          }
        })
      }else {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-warning',
            message: '无法激活',
            description: "没有输入序列号",
          });
        }, 100)
      }
    },
    getVulnerabilities(type){
      getVulnerabilities().then(res =>{
        let white = res.white
        let black = res.black
        if(white && type !==2){
          this.vulnerabilities.whiteList = white.split(",")
        }
        if(black && type !==1){
          this.vulnerabilities.blackList = black.split(",")
        }
      })
    },
    removeWhite (uuid){
      removeVulnerabilitiesWhite({white: uuid}).then(res=>{
        this.successMsg(uuid + "从白名单移除成功")
      }).finally(() => {
        this.getVulnerabilities(1)
      })
    },
    removeBlack (uuid){
      removeVulnerabilitiesBlack({black: uuid}).then(res=>{
        this.successMsg(uuid + "从黑名单移除成功")
      }).finally(() => {
        this.getVulnerabilities(2)
      })
    },
    addWhite (uuid){
      addVulnerabilitiesWhite({white: uuid}).then(res=>{
        this.successMsg(uuid + "添加到白名单成功")
      }).catch((err)=> {
        this.$notification["error"]({
          message: err.response.data.error,
          description: ""
        })
      }).finally(() => {
        this.uuid = ""
        this.showVulnerabilitiesModal = false
        this.getVulnerabilities(1)
      })
    },
    addBlack (uuid){
      addVulnerabilitiesBlack({black: uuid}).then(res=>{
        this.successMsg(uuid + "添加到黑名单成功")
      }).catch((err)=> {
        this.$notification["error"]({
          message: err.response.data.error,
          description: ""
        })
      }).finally(() => {
        this.uuid = ""
        this.showVulnerabilitiesModal = false
        this.getVulnerabilities(2)
      })
    },
    vulnerabilitiesModalCancel(){
      this.showVulnerabilitiesModal = false
    },
    addVulnerabilities(){
      if(!this.uuid){
        this.$notification["warning"]({
          message: '请输入漏洞编号',
          description: ""
        })
        return
      }
      if(this.vulnerabilitiesType === 1){
        this.addWhite(this.uuid)
      }else if(this.vulnerabilitiesType === 2){
        this.addBlack(this.uuid)
      }
    },
    successMsg(message){
      if(!message){
        message = "操作成功"
      }
      this.$notification["success"]({
        message: message,
        description: ""
      })
    }
  },
};
</script>

<style lang="scss" scoped>
#settings::v-deep {
  .ant-list {
    width: 100%;
  }

  .ant-list-item-meta-avatar {
    margin-right: 8px;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-ink::before {
    display: none;
  }

  .ant-anchor-link {
    padding: 0;
    margin-top: 8px;

    .ant-anchor-link-title-active{
      background-color: #eeeeee;
    }
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    background-color: #eeeeee;
  }

  .ant-anchor-link a svg g {
    fill: #344767;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .card-profile-head {
    margin: 0 0 24px;
  }

  .tags-field .ant-form-item-control {
    line-height: 33px;
  }

  .form-tag.ant-tag {
    border-radius: 20px;
    padding: 4px 10px;
    font-size: 12px;
    font-weight: 500;
    margin-right: 3.75px;
    margin-bottom: 3.75px;
    background-color: #3a416f;
    border: 1px solid #3a416f;
    color: #fff;
  }

  .form-tag.ant-tag .anticon-close {
    color: #fff;
    height: 16px;
    border-left: 1px solid hsla(0, 0%, 100%, .3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: .75;
  }

  .form-tag.ant-tag .anticon-close:hover {
    color: #fff;
    opacity: 1;
  }

  .tags-field .ant-input {
    margin-bottom: 5px;
    margin-top: 4px;
  }

  .tags-field .ant-select {
    .ant-select-selection__choice__remove i {
      color: #fff;
      height: 16px;
      border-left: 1px solid hsla(0, 0%, 100%, .3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: .75;

      &:hover {
        color: #fff;
        opacity: 1;
      }
    }

    .ant-select-selection__rendered > ul > li:not(.ant-select-search) {
      border-radius: 20px;
      padding: 2px 27px 2px 10px;
      font-size: 12px;
      font-weight: 500;
      margin-right: 3.75px;
      margin-bottom: 3.75px;
      background-color: #3a416f;
      border: 1px solid #3a416f;
      color: #fff;
      line-height: 2;
      height: 30px;
    }

    .ant-select-selection--multiple {
      padding: 8px 10px;
    }
  }
  .white-group,.black-group {
    width:100%;
    display: inline-flex;
    justify-content: flex-start;
    flex-wrap: wrap;
  }

  .white-group .ant-list-item-main,.black-group .ant-list-item-main{
    min-width: unset;
  }

  .white-group .white,.black-group .black{
    margin-right: 10px;
    margin-bottom: 10px;
    width: calc((100% - 50px) / 5);
  }

  .white-card,.black-card{
    height:100%;
    margin-right: 10px;
    width: calc((100% - 20px) / 2);
    overflow-y: auto;
  }
  .white-group .uuid,.black-group .uuid{
    font-size: 5px;
  }

  .d-popconfirm{
    height: 34px;
    font-size: 12px;
    font-weight: 600;
    margin-right: 20px;
  }

  .d-popconfirm > svg + span {
    vertical-align: middle;
    display: inline-block;
    transition: margin-left 0.3s cubic-bezier(0.645, 0.045, 0.355, 1);
    pointer-events: none;
  }
  .d-popconfirm svg {
    vertical-align: middle;
    margin-right: 5px;
  }

  .o-btn {
    width: 36px;
    height: 36px;
    margin-right: 8px;
    background-color: #1890FF;
    border-radius: 8px;
    display: inline-flex;
    justify-content: center;
    align-items: center;
  }
  .o-btn img{
    width: 20px;
    height: 20px;
    cursor: pointer;
  }

  .o-black{
    background-color: #f58080
  }
}
</style>