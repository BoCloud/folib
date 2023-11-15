<template>
  <div id="settings">
    <a-tabs class="tabs-sliding"
            default-active-key="1"
            @change="tabChange($event)">
      <a-tab-pane key="1"
                  tab="全局配置">
        <a-row type="flex"
               :gutter="[24, 24]">
          <a-col :span="24"
                 :lg="6">
            <!-- Page Anchors -->
            <a-affix :offset-top="navbarFixed ? 100 : 10">
              <a-card :bordered="false"
                      class="header-solid mb-24">
                <a-anchor :targetOffset="navbarFixed ? 100 : 10"
                          :affix="false"
                          @click="handleClick">
                  <a-anchor-link href="#basic">
                    <div slot="title"
                         class="ant-list-item-meta">
                      <a-icon type="snippets"
                              theme="filled"
                              class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">基础信息配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#smtp">
                    <div slot="title"
                         class="ant-list-item-meta">
                      <a-icon type="unlock"
                              theme="filled"
                              class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">SMTP配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#proxy">
                    <div slot="title"
                         class="ant-list-item-meta">
                      <a-icon type="safety-certificate"
                              theme="filled"
                              class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">网络代理配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#cors">
                    <div slot="title"
                         class="ant-list-item-meta">
                      <a-icon type="dashboard"
                              theme="filled"
                              class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">CORS配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                  <a-anchor-link href="#advance">
                    <div slot="title"
                         class="ant-list-item-meta">
                      <a-icon type="setting"
                              theme="filled"
                              class="text-gray-6 text-lg" />
                      <h4 class="ant-list-item-meta-title">
                        <span class="font-regular">高级配置</span>
                      </h4>
                    </div>
                  </a-anchor-link>
                </a-anchor>
              </a-card>
            </a-affix>
            <!-- / Page Anchors -->

          </a-col>
          <a-col :span="24"
                 :lg="18">
            <!-- Basic Info card -->
            <a-card :bordered="false"
                    id="basic"
                    class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">基础 配置</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="应用名称"
                                 :colon="false">
                      <a-input placeholder="folib"
                               v-model="serverSettings.instanceName" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="制品传输切片大小（MB）"
                                 :colon="false">
                      <a-input placeholder="MB" type="number"
                               v-model="serverSettings.kbps" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="Base Url"
                                 :colon="false">
                      <a-input placeholder="http://localhot:38080"
                               v-model="serverSettings.baseUrl" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="端口号"
                                 :colon="false">
                      <a-input placeholder="38080"
                               v-model="serverSettings.port" />
                    </a-form-item>
                  </a-col>
                </a-row>
                
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>应用名称修改会自动修改到配置文件</li>
                  <li>网络限速，不设置则不限速</li>
                  <li>baseurl,如果你使用了反向代理公网等情况下可以使用它</li>
                  <li>{{instanceName}}-Server服务的后端通信端口</li>
                </ul>
              </a-form>
            </a-card>
            <!-- / Basic Info card -->

            <!-- Change Password card -->
            <a-card :bordered="false"
                    id="smtp"
                    class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">SMTP配置</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="用户名"
                                 :colon="false">
                      <a-input placeholder="SMTP用户名"
                               v-model="serverSettings.smtpConfigurationForm.username" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="密码"
                                 :colon="false">
                      <a-input-password placeholder="SMTP密码"
                                        autocomplete="new-password"
                                        v-model="serverSettings.smtpConfigurationForm.password" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="HOST"
                                 :colon="false">
                      <a-input placeholder="HOST"
                               v-model="serverSettings.smtpConfigurationForm.host" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="6">
                    <a-form-item class="mb-10"
                                 label="端口"
                                 :colon="false">
                      <a-input placeholder="端口"
                               v-model="serverSettings.smtpConfigurationForm.port" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="6">
                    <a-form-item class="mb-10"
                                 label="协议类型"
                                 :colon="false">
                      <a-select v-model="serverSettings.smtpConfigurationForm.connection"
                                show-search
                                :allowClear="true"
                                placeholder="协议类型"
                                option-filter-prop="children"
                                :filter-option="filterOption">
                        <a-select-option value="plain">
                          Plain
                        </a-select-option>
                        <a-select-option value="ssl">
                          SSL
                        </a-select-option>
                        <a-select-option value="tls">
                          TLS
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>该配置是用来设置系统邮件</li>
                  <li>程序中某些事件会对相关用户进行邮件通知</li>
                </ul>
              </a-form>
            </a-card>

            <!-- Two-factor authentication card -->
            <a-card :bordered="false"
                    id="proxy"
                    class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">网络代理</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="用户名"
                                 :colon="false">
                      <a-input placeholder="代理用户名"
                               v-model="serverSettings.proxyConfigurationForm.username" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="密码"
                                 :colon="false">
                      <a-input-password placeholder="代理密码"
                                        autocomplete="new-password"
                                        v-model="serverSettings.proxyConfigurationForm.password" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="12">
                    <a-form-item class="mb-10"
                                 label="代理地址"
                                 :colon="false">
                      <a-input placeholder="代理地址"
                               v-model="serverSettings.proxyConfigurationForm.host" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="6">
                    <a-form-item class="mb-10"
                                 label="端口"
                                 :colon="false">
                      <a-input placeholder="端口"
                               v-model="serverSettings.proxyConfigurationForm.port" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="6">
                    <a-form-item class="mb-10"
                                 label="类型"
                                 :colon="false">
                      <a-select v-model="serverSettings.proxyConfigurationForm.type"
                                show-search
                                placeholder="类型"
                                :allowClear="true"
                                option-filter-prop="children"
                                :filter-option="filterOption">
                        <a-select-option value="HTTP">
                          HTTP
                        </a-select-option>
                        <a-select-option value="HTTPS">
                          HTTPS
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

            <a-card :bordered="false"
                    id="cors"
                    class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">CORS配置</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="16">
                    <a-form-item class="tags-field mb-10"
                                 label="Origins"
                                 :colon="false">
                      <a-select mode="tags"
                                v-model="serverSettings.corsConfigurationForm.allowedOrigins"
                                @change="allowedOriginsChange"
                                style="width: 100%"
                                placeholder="例如：*">
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="开启所有"
                                 :colon="false">
                      <span class="mr-15">开启</span>
                      <a-switch v-model="serverSettings.corsConfigurationForm.corsAllowAll"
                                @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>开启所有意味着不再有跨域限制</li>
                </ul>
              </a-form>
            </a-card>

            <a-card :bordered="false"
                    id="advance"
                    class="header-solid mb-24">
              <template #title>
                <h5 class="mb-0 font-semibold">高级配置</h5>
              </template>
              <a-form :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="允许匿名访问"
                                 :colon="false">
                      <span class="mr-15">开启</span>
                      <a-switch v-model="serverSettings.advancedConfigurationForm.allowAnonymous"
                                @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         :lg="8">
                    <a-form-item class="mb-10"
                                 label="显示校验文件"
                                 :colon="false">
                      <span class="mr-15">开启</span>
                      <a-switch v-model="serverSettings.advancedConfigurationForm.showChecksum"
                                @change="corsAllowAllChange" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <p>说明:</p>
                <ul class="pl-15 text-muted">
                  <li>开启允许匿名访问，匿名用户可访问公开库及结构，关闭后匿名用户必须登录方可正常使用。</li>
                  <li>开启显示校验文件，仓库详情页会展示校验文件，关闭后仓库详情页则不会展示校验文件。</li>
                </ul>
              </a-form>
            </a-card>
            
            <a-card :bordered="false"
                    id="delete-account"
                    class="header-solid mb-24">
              <a-form id="components-form-demo-normal-login"
                      class="login-form list-settings-sessions"
                      :hideRequiredMark="true">
                <a-row type="flex"
                       align="middle">
                  <a-col style="min-width: 40px;"
                         class="text-center">
                    <!--										<a-switch></a-switch>-->
                  </a-col>
                  <a-col class="pl-15">
                    <p class="mb-0 font-semibold">保存操作</p>
                    <small class="text-dark">该保存按钮将会针对以上4个部分的修改统一保存</small>
                  </a-col>
                  <a-col :span="24"
                         :md="12"
                         class="ml-auto"
                         style="display: flex; align-items: center; justify-content: flex-end">
                    <a-button @click="getServerSettings">
                      取 消
                    </a-button>
                    <a-button type="danger"
                              class="ml-10"
                              @click="saveServerSettings">
                      保 存
                    </a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </a-col>
        </a-row>
      </a-tab-pane>
      <a-tab-pane key="2"
                  tab="安全策略">
        <!-- <a-row type="flex" :gutter="[24]"> -->
        <!-- <a-col :span="24" :lg="24"> -->
        <a-tabs class="tabs-sliding card-container"
                default-active-key="1"
                @change="vulnerabilityTabChange($event)">
          <a-tab-pane key="1"
                      tab="白名单">
            <a-card class="header-solid white-card"
                    id="white">
              <!-- <template #title>
                    <p>白名单</p>
                  </template> -->
              <div class="o-btn"
                   @click="() => (showVulnerabilitiesModal = true, vulnerabilitiesType = 1)">
                <img src="images/folib/white.svg" />
              </div>
              <div class="white-group">
                <a-list item-layout="vertical"
                        size="large"
                        :data-source="vulnerabilities.whiteList"
                        :pagination="vulnerabilities.whiteList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.whiteList.length, showLessItems: true }">
                  <a-list-item slot="renderItem"
                               :key="index"
                               slot-scope="item, index">
                    <label>{{ item }}</label>
                    <template #extra>
                      <a-popconfirm title="确定要从白名单移除吗？"
                                    ok-text="确定"
                                    cancel-text="取消"
                                    class="d-popconfirm"
                                    @confirm="removeWhite(item)">
                        <svg width="16"
                             height="16"
                             viewBox="0 0 20 20"
                             fill="none"
                             xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-danger"
                                fill-rule="evenodd"
                                clip-rule="evenodd"
                                d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                fill="#111827" />
                        </svg>
                        <span class="text-danger">DELETE</span>
                      </a-popconfirm>
                    </template>
                  </a-list-item>
                </a-list>
              </div>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="2"
                      tab="黑名单">
            <a-card class="header-solid black-card"
                    id="black">
              <!-- <template #title>
                    <p>黑名单</p>
                  </template> -->
              <div class="o-btn o-black"
                   @click="() => (showVulnerabilitiesModal = true, vulnerabilitiesType = 2)">
                <img src="images/folib/black.svg" />
              </div>
              <div class="black-group">
                <a-list item-layout="vertical"
                        size="large"
                        :data-source="vulnerabilities.blackList"
                        :pagination="vulnerabilities.blackList.length === 0 ? false : { pageSize: 5, total: vulnerabilities.blackList.length, showLessItems: true }">
                  <a-list-item slot="renderItem"
                               :key="index"
                               slot-scope="item, index">
                    {{ item }}
                    <template #extra>
                      <a-popconfirm title="确定要从黑名单移除吗？"
                                    ok-text="确定"
                                    cancel-text="取消"
                                    class="d-popconfirm"
                                    @confirm="removeBlack(item)">
                        <svg width="16"
                             height="16"
                             viewBox="0 0 20 20"
                             fill="none"
                             xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-danger"
                                fill-rule="evenodd"
                                clip-rule="evenodd"
                                d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                fill="#111827" />
                        </svg>
                        <span class="text-danger">DELETE</span>
                      </a-popconfirm>
                    </template>
                  </a-list-item>
                </a-list>
              </div>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="3"
                      tab="通知设置">
            <a-card class="header-solid"
                    id="notice">
              <a-form :form="ruleForm"
                      ref="ruleForm"
                      layout="vertical"
                      :wrapper-col="{ span: 8 }"
                      @submit.prevent="ruleFormSubmit">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-form-item class="mb-10"
                                 label="通知漏洞等级">
                      <a-checkbox-group v-decorator="['levels',
                        {
                          rules: [
                            { required: true, message: '请选择漏洞等级', type: 'array' },
                          ],
                        },
                      ]"
                                        style="width: 100%;">
                        <a-row>
                          <a-col :span="6">
                            <a-checkbox value="CRITICAL">
                              严重
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="HIGH">
                              高危
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="MEDIUM">
                              中危
                            </a-checkbox>
                          </a-col>
                          <a-col :span="6">
                            <a-checkbox value="LOW">
                              低危
                            </a-checkbox>
                          </a-col>
                        </a-row>
                      </a-checkbox-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24"
                         class="text-left">
                    <a-form-item class="mb-10"
                                 label="通知策略">
                      <a-checkbox-group v-decorator="['notifyScopes',
                      ]"
                                        style="width: 100%;">
                        <a-row>
                          <a-col :span="12">
                            <a-checkbox value="admin">
                              通知平台管理员
                            </a-checkbox>
                          </a-col>
                          <a-col :span="12"
                                 class="text-right">
                            <a-checkbox value="storageAdmin">
                              通知存储空间管理员
                            </a-checkbox>
                          </a-col>
                        </a-row>
                      </a-checkbox-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24">
                    <a-form-item class="tags-field mb-10"
                                 label="指定用户">
                      <a-select mode="multiple"
                                style="width: 100%"
                                show-search
                                placeholder="请选择用户"
                                v-decorator="['receiverUsers',
                      ]">
                        <a-select-option v-for="(user, index) in userList"
                                         :key="index"
                                         :value="user.username">
                          {{ user.username }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="24">
                    <a-form-item class="tags-field mb-10"
                                 label="指定邮箱">
                      <a-select mode="tags"
                                style="width: 100%"
                                notFoundContent=""
                                placeholder="请输入邮箱"
                                v-decorator="['receiverEmails',
                      ]">
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12"
                         class="text-right">
                    <a-button key="submit"
                              class="px-30"
                              size="small"
                              type="primary"
                              htmlType="submit">保存</a-button>
                  </a-col>
                  <a-col :span="12"
                         class="text-left">
                    <a-button key="back"
                              class="px-30 ml-10"
                              size="small"
                              @click="ruleFormCancel()">取消</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </a-tab-pane>
          <a-tab-pane key="4"
                      tab="阻断设置">
            <a-card>
              <a-row :gutter="16">
                <a-col :span="14">
                  <a-card class="header-solid block block-settings">
                    <a-form class="block-form"
                            ref="blockForm"
                            :form="blockForm"
                            layout="horizontal"
                            :wrapper-col="{ span: 24 }"
                            @submit.prevent="blockFormSubmit">
                      <a-row :gutter="[24]">
                        <a-col :span="24"
                               class="text-left">
                          <a-form-item class="mb-10"
                                       label=""
                                       :wrapper-col="{ span: 24 }">
                            <a-radio-group v-decorator="['blockType',
                              {
                                rules: [
                                  { required: true, message: '请选择阻断方式' },
                                ],
                              },
                            ]"
                                           style="width: 100%;"
                                           @change="blockTypeChange($event.target.value)">
                              <a-row>
                                <a-col :span="6">
                                  <a-radio :value="1">
                                    全量阻断 <span class="tips">（tips：此种阻断方式会自动过滤黑名单）</span>
                                  </a-radio>
                                </a-col>
                                <a-row class="block-full mt-30">
                                  <a-col :span="24">
                                    <a-form-item class=""
                                                 label="漏洞等级"
                                                 :label-col="{ span: 4 }"
                                                 :wrapper-col="{ span: 12 }">
                                      <a-checkbox-group v-decorator="['blockLevels',
                                        {
                                          rules: [
                                            { required: false, message: '请选择漏洞等级', type: 'array' },
                                          ],
                                        },
                                      ]"
                                                        style="width: 100%;">
                                        <a-row>
                                          <a-col :span="6">
                                            <a-checkbox value="CRITICAL">
                                              严重
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="HIGH">
                                              高危
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="MEDIUM">
                                              中危
                                            </a-checkbox>
                                          </a-col>
                                          <a-col :span="6">
                                            <a-checkbox value="LOW">
                                              低危
                                            </a-checkbox>
                                          </a-col>
                                        </a-row>
                                      </a-checkbox-group>
                                    </a-form-item>
                                  </a-col>
                                  <a-col :span="24">
                                    <a-form-item class=""
                                                 label="过滤白名单"
                                                 :label-col="{ span: 4 }"
                                                 :wrapper-col="{ span: 1 }">
                                      <a-switch v-decorator="['filterWhites',
                                        {
                                          valuePropName: 'checked',
                                          rules: [
                                            { required: false },
                                          ],
                                        },
                                      ]"
                                                style="width: 100%;">

                                      </a-switch>
                                    </a-form-item>
                                  </a-col>
                                </a-row>
                                <a-col class="mt-30"
                                       :span="24">
                                  <a-radio :value="2">
                                    黑名单阻断 <span class="tips">（tips：此种阻断方式会自动过滤白名单）</span>
                                  </a-radio>
                                </a-col>
                                <a-col class="mt-30"
                                       :span="24">
                                  <a-radio :value="3">
                                    包名阻断 <span class="tips">（tips：此种阻断方式会按照包名拦截）</span>
                                  </a-radio>
                                  <a-tooltip v-if="packageNameShow"
                                             @click="packageNameModalShow">
                                    <template slot="title">新增</template>
                                    <a-icon type="plus-circle"
                                            theme="filled"
                                            class="cursor-pointer package-name-add"
                                            :style="{ fontSize: '28px', color: '#1890FF' }" />
                                  </a-tooltip>
                                </a-col>
                              </a-row>
                            </a-radio-group>
                          </a-form-item>
                        </a-col>
                        <a-col :span="12"
                               class="text-right mt-50"
                               v-if="!packageNameShow">
                          <a-button key="submit"
                                    class="px-30"
                                    size="small"
                                    type="primary"
                                    htmlType="submit">保存</a-button>
                        </a-col>
                        <a-col :span="12"
                               class="text-left mt-50"
                               v-if="!packageNameShow">
                          <a-button key="back"
                                    class="px-30 ml-10"
                                    size="small"
                                    @click="blockFormCancel()">取消</a-button>
                        </a-col>
                      </a-row>
                    </a-form>
                  </a-card>
                </a-col>
                <a-col :span="24">
                  <package-name v-if="packageNameShow" ref="packageName"/>
                </a-col>
              </a-row>
            </a-card>
          </a-tab-pane>
        </a-tabs>
        <!-- </a-col> -->
        <!-- </a-row> -->
      </a-tab-pane>
      <a-tab-pane key="3"
                  tab="LDAP配置">
        <div class="mx-auto mt-50"
             style="max-width: 1000px;">
          <div class="mb-50"
               style="max-width: 1000px;">

            <a-steps progress-dot
                     v-model="step">
              <a-step title="连接配置" />
              <a-step title="用户映射" />
              <a-step title="角色映射" />
            </a-steps>
          </div>

          <div class="mb-24">
            <!-- Step 1 -->
            <a-card v-if="step == 0"
                    :bordered="false"
                    class="header-solid"
                    :bodyStyle="{ paddingTop: 0 }"
                    :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">连接配置</h5>
                <p class="font-regular">该部分配置用于和LDAP建立连接</p>
              </template>
              <a-form @submit="handleSubmit"
                      :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-form-item class="mb-10"
                                 label="URL"
                                 :colon="false">
                      <a-input placeholder="例如: ldap://1.2.3.4/dc=domain,dc=com"
                               v-model="ldap.url" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="绑定DN"
                                 :colon="false">
                      <a-input placeholder="例如:cn=manager,ou=users,dc=domain,dc=com"
                               v-model="ldap.managerDn" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="绑定密码"
                                 :colon="false">
                      <a-input placeholder="********"
                               v-model="ldap.managerPassword" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="是否开启LDAP服务"
                                 :colon="false">
                      <span class="mr-15">{{ ldap.enableProvider ? '开启' : '关闭' }}</span>
                      <a-switch default-checked
                                v-model="ldap.enableProvider" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                  </a-col>
                  <a-col :span="12"
                         class="text-right">
                    <a-button type="primary"
                              @click="moveStep(1)"
                              class="px-25">下一步</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 2 -->
            <a-card v-else-if="step == 1"
                    :bordered="false"
                    class="header-solid"
                    :bodyStyle="{ paddingTop: 0 }"
                    :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">用户映射</h5>
              </template>
              <a-form @submit="handleSubmit"
                      :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="用户搜索对象"
                                 :colon="false">
                      <a-input placeholder="例如：ou=Users"
                               v-model="ldap.userSearchBase" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="用户过滤条件"
                                 :colon="false">
                      <a-input placeholder="例如：(uid={0})"
                               v-model="ldap.userSearchFilter" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="16">
                    <a-form-item class="tags-field mb-10"
                                 label="在验证查找用户时将使用以下用户DN列表"
                                 :colon="false">
                      <a-select mode="tags"
                                v-model="ldap.userDnPatternList"
                                :defaultValue="ldap.userDnPatternList"
                                style="width: 100%"
                                placeholder="例如：uid={0},uid={0},ou=Admins">
                        <a-select-option v-for="(tag, index) in ldap.userDnPatternList"
                                         :key="index"
                                         :value="tag">
                          {{ tag }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10"
                                 label="用户密码是否是Base64加密?"
                                 :colon="false">
                      <span class="mr-15">{{ ldap.userPasswordEncoded ? '是' : '否' }}</span>
                      <a-switch default-checked
                                v-model="ldap.userPasswordEncoded" />
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)"
                              class="px-25">上一步</a-button>
                  </a-col>
                  <a-col :span="12"
                         class="text-right">
                    <a-button type="primary"
                              @click="moveStep(1)"
                              class="px-25">下一步</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
            <!-- Step 3 -->
            <a-card v-else-if="step == 2"
                    :bordered="false"
                    class="header-solid"
                    :bodyStyle="{ paddingTop: 0 }"
                    :headStyle="{ paddingBottom: '0' }">
              <template #title>
                <h5 class="mb-0">角色匹配</h5>
              </template>
              <a-form @submit="handleSubmit"
                      :hideRequiredMark="true">
                <a-row :gutter="[24]">
                  <a-col :span="8">
                    <a-form-item class="mb-10"
                                 label="Group匹配"
                                 :colon="false">
                      <a-input placeholder="例如：ou=Groups"
                               v-model="ldap.authorities.groupSearchBase" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="8">
                    <a-form-item class="mb-10"
                                 label="Group过滤条件"
                                 :colon="false">
                      <a-input placeholder="例如：(uniqueMember={0})"
                               v-model="ldap.authorities.groupSearchFilter" />
                    </a-form-item>
                  </a-col>
                  <a-col :span="6">
                    <a-form-item class="mb-10"
                                 label="角色属性"
                                 :colon="false">
                      <a-input placeholder="例如：cn,ou"
                               v-model="ldap.authorities.groupRoleAttribute" />
                    </a-form-item>

                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]"
                       v-for="(item, index) in ldap.roleMappingList"
                       :key="index">
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="FOLIB角色"
                                 :colon="false">
                      <a-select v-model="item.folibRole">
                        <a-select-option v-for="(i, index) in assignableRoles"
                                         :key="index"
                                         :value="i.name">
                          {{ i.name }}
                        </a-select-option>
                      </a-select>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item class="mb-10"
                                 label="LDAP角色"
                                 :colon="false">
                      <a-col :span="20">
                        <a-input placeholder="输入LDAP的角色"
                                 v-model="item.externalRole" />
                      </a-col>
                      <a-col :span="4">
                        <a-button type="link"
                                  size="small"
                                  @click="roleMappingDelHandle(item, index)">
                          <svg width="16"
                               height="16"
                               viewBox="0 0 20 20"
                               fill="none"
                               xmlns="http://www.w3.org/2000/svg">
                            <path class="fill-danger"
                                  fill-rule="evenodd"
                                  clip-rule="evenodd"
                                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                  fill="#111827" />
                          </svg>
                        </a-button>
                      </a-col>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-row :gutter="[24]">
                  <a-col :span="24">
                    <a-button type="dashed"
                              style="width: 91.66666%"
                              @click="roleMappingAddHandle">
                      <a-icon type="plus" />
                      添加
                    </a-button>
                  </a-col>
                </a-row>
                <hr class="gradient-line">
                <a-row :gutter="[24]">
                  <a-col :span="12">
                    <a-button @click="moveStep(-1)"
                              class="px-25">上一步</a-button>
                  </a-col>
                  <a-col :span="12"
                         class="text-right">
                    <a-button type="primary"
                              class="px-25"
                              @click="putLdap">完成</a-button>
                  </a-col>
                </a-row>
              </a-form>
            </a-card>
          </div>
        </div>
      </a-tab-pane>
      <a-tab-pane key="4"
                  tab="节点许可配置">
        <a-row type="flex"
               :gutter="24">

          <!-- 许可信息-->
          <a-col :span="24"
                 :md="12"
                 class="mb-24">

            <a-card :bordered="false"
                    class="header-solid h-full card-profile-information"
                    :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
                    :headStyle="{ paddingRight: 0, }">
              <template #title>
                <h6 class="font-semibold m-0">FOLIB 许可信息</h6>
              </template>
              <a-button v-if="machineInfo.haveError || machineInfo.dalyOut"
                        type="link"
                        slot="extra"
                        @click="copy(machineInfo.mac)">
                <a-icon type="copy"
                        theme="twoTone" />
              </a-button>
              <p class="text-dark"
                 v-if="!machineInfo.haveError && !machineInfo.dalyOut">
                尊敬的用户,很荣幸您选择使用{{instanceName}}!
                在接下来{{instanceName}} 将会为您提供统一软件包管理。
                IT数字化转型道路长远,{{instanceName}} 与您随行！
              </p>
              <p class="text-dark"
                 v-if="machineInfo.haveError && machineInfo.dalyOut">
                尊敬的用户,很荣幸您选择使用{{instanceName}}! 如果觉得符合您企业信创发展战略,可选择购买正式版本。我们的服务热线：400-991-5355
              </p>
              <p class="text-dark"
                 v-if="(!machineInfo.haveError) && machineInfo.dalyOut">
                尊敬的用户,很荣幸您选择使用{{ instanceName }}! 您的序列号已经过期，为了更好的为您提供服务请尽快续期。我们的服务热线：400-991-5355
              </p>
              <hr class="my-25">
              <a-descriptions :title="machineInfo.haveError ? '未激活' : (!machineInfo.haveError) && machineInfo.dalyOut ? '已过期' : '已激活'"
                              :column="1">
                <a-descriptions-item label="机器码">
                  {{ machineInfo.mac }}
                </a-descriptions-item>
                <a-descriptions-item label="版本类型">
                  {{ machineInfo.haveError || !machineInfo.object ? "无" : machineInfo.object.type }}
                </a-descriptions-item>
                <a-descriptions-item label="有效日期">
                  {{ machineInfo.haveError || !machineInfo.object ? "无" : machineInfo.object.endDate }}
                </a-descriptions-item>
                <a-descriptions-item label="序列号">
                  {{ machineInfo.haveError || !machineInfo.object ? "无" : machineInfo.object.codes }}
                </a-descriptions-item>
                <a-descriptions-item label="功能等级">
                  <a-tag> {{ machineInfo.haveError || !machineInfo.object ? "无" : upperCase(machineInfo.object.level) }}</a-tag>
                </a-descriptions-item>
                <a-descriptions-item label="是否激活">
                  <a href="http://folib.com"
                     class="mx-5 px-5"
                     v-if="!machineInfo.haveError">
                    <a-avatar :size="24"
                              shape="square"
                              src="images/folib/isactivate.svg" />
                  </a>
                  <a href="http://folib.com"
                     class="mx-5 px-5"
                     v-if="machineInfo.haveError">
                    <a-avatar :size="24"
                              shape="square"
                              src="images/folib/notactivate.svg" />
                  </a>
                </a-descriptions-item>
              </a-descriptions>
            </a-card>

          </a-col>
          <a-col :span="24"
                 :md="12"
                 class="mb-24">
            <a-card :bordered="false"
                    class="header-solid h-full card-profile-information"
                    :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }"
                    :headStyle="{ paddingRight: 0, }">
              <template #title>
                <h6 class="font-semibold m-0">激活序列号</h6>
              </template>
              <a-button type="link"
                        slot="extra"
                        @click="postActivate(false)">
                <svg width="20"
                     height="20"
                     viewBox="0 0 20 20"
                     fill="none"
                     xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted"
                        d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                        fill="#111827" />
                  <path class="fill-muted"
                        d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                        fill="#111827" />
                </svg>
                正式激活
              </a-button>
              <a-button type="link"
                        slot="extra"
                        @click="postActivate(true)">
                <svg width="20"
                     height="20"
                     viewBox="0 0 20 20"
                     fill="none"
                     xmlns="http://www.w3.org/2000/svg">
                  <path class="fill-muted"
                        d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                        fill="#111827" />
                  <path class="fill-muted"
                        d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                        fill="#111827" />
                </svg>
                试用
              </a-button>
              <p class="$color-muted">
                你可以复制左侧机器码来获取FOLIB的序列号，并将序列号填入下方。正式激活请确保激活过程可以联网，如果需要开通网络策略，请将"license.folib.com"设置为白名单,如果想通过离线方式激活请联系我司技术服务
              </p>
              <hr class="my-25">
              <a-form-item class="mb-10"
                           label="序列号"
                           :colon="false">
                <a-textarea :rows="8"
                            placeholder="请粘贴本机器码的序列号"
                            v-model="activateCode" />
              </a-form-item>
            </a-card>
            <!-- / Conversations Card -->

          </a-col>

        </a-row>
      </a-tab-pane>

      <!--单点登录配置-->
      <a-tab-pane key="8"
                  tab="SSO配置">
        <a-card class="header-solid block">
          <a-col :span="24"
                 :md="24"
                 class="mb-24 text-right">

            <div class="mx-25 mb-50">
              <a-col :span="24"
                     class="text-right">
                <a-tooltip @click="()=>ssoAdd()">
                  <template slot="title">新增</template>
                  <a-icon type="plus-circle"
                          theme="filled"
                          class="cursor-pointer"
                          :style="{ fontSize: '28px', color: '#1890FF' }" />
                </a-tooltip>
              </a-col>
            </div>
            <a-table :columns="ssoColumns"
                     :data-source="ssoList"
                     :pagination="false"
                     row-key="clientId">

              <template slot="operation"
                        slot-scope="text, record"  >
                <div class="editable-row-operations"  style="width:100px">
                  <span>
                    <a @click="() => ssoEdit(record)">Edit</a>
                  </span>
                  <a-divider type="vertical" />
                  <span>
                    <a-popconfirm
                      title="确定删除这条记录?"
                      okText="确定"
                      cancelText="取消"
                      @confirm="() => ssoDelete(record)"
                      >
                    <a>delete</a>
                  </a-popconfirm>
                  </span>
                </div>
              </template>

            </a-table>

          </a-col>

        </a-card>
      </a-tab-pane>
      <a-tab-pane key="5"
                  tab="元数据配置">
        <a-card class="header-solid block">
          <div class="mx-25 mb-50">
            <a-col :span="24"
                   class="text-right">
              <a-tooltip @click="metadataHandler(1)">
                <template slot="title">新增</template>
                <a-icon type="plus-circle"
                        theme="filled"
                        class="cursor-pointer"
                        :style="{ fontSize: '28px', color: '#1890FF' }" />
              </a-tooltip>
            </a-col>
          </div>
          <a-table :columns="metadataColumns"
                   :scroll="{ x: true }"
                   :data-source="metadataList"
                   :row-key="(r, i) => i.toString()">
            <div slot="type"
                 slot-scope="type">
              <span v-for="(item, index) in metadataTypes"
                    :key="index">
                <span v-if="type === item.value">{{ item.label }}</span>
              </span>
            </div>
            <div slot="viewShow"
                 slot-scope="viewShow">
              {{ viewShow === 1 ? '展示' : '不展示' }}
            </div>
            <div slot="operation"
                 slot-scope="text, record">
              <div class="col-action">
                <a-popconfirm title="确定要删除吗？"
                              okType="danger"
                              ok-text="确定"
                              cancel-text="取消"
                              @confirm="metadataHandlerDelete(record)">
                  <a-button type="link"
                            size="small">
                    <svg width="16"
                         height="16"
                         viewBox="0 0 20 20"
                         fill="none"
                         xmlns="http://www.w3.org/2000/svg">
                      <path class="fill-danger"
                            fill-rule="evenodd"
                            clip-rule="evenodd"
                            d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                            fill="#111827" />
                    </svg>
                    <span class="text-danger">DELETE</span>
                  </a-button>
                </a-popconfirm>
                <a-button type="link"
                          size="small"
                          @click="metadataHandler(2, record)">
                  <svg width="16"
                       height="16"
                       viewBox="0 0 20 20"
                       fill="none"
                       xmlns="http://www.w3.org/2000/svg">
                    <path class="fill-muted"
                          d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                          fill="#111827" />
                    <path class="fill-muted"
                          d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                          fill="#111827" />
                  </svg>
                  <span class="text-dark">EDIT</span>
                </a-button>
              </div>
            </div>
          </a-table>
        </a-card>
      </a-tab-pane>
      <a-tab-pane key="6"
                  tab="节点分发配置">
        <a-tabs class="tabs-sliding"
            default-active-key="1"
            @change="tabChange($event)">
            <a-tab-pane key="1"
              tab="内部节点">
              <a-card class="header-solid block">
                <div class="mx-25 mb-50">
                  <a-col :span="24"
                        class="text-right">
                    <a-tooltip @click="artifactDispatchHandler(1)">
                      <template slot="title">新增</template>
                      <a-icon type="plus-circle"
                              theme="filled"
                              class="cursor-pointer"
                              :style="{ fontSize: '28px', color: '#1890FF' }" />
                    </a-tooltip>
                  </a-col>
                </div>
                <a-table :columns="artifactDispatchColumns"
                        :scroll="{ x: true }"
                        :data-source="artifactDispatchList"
                        :row-key="(r, i) => i.toString()">
                  <div slot="isThisCluster"
                      slot-scope="text, record">
                    {{ record.isThisCluster === true ? '是' : '否' }}
                  </div>
                  <div slot="wsClientOnline"
                      slot-scope="text, record">
                    <span v-if="record.wsClientOnline && record.wsClientOnline === true" class="text-success">在线</span>
                    <span v-else class="text-danger">离线</span>
<!--                    {{ record.online && record.online === true ? '在线' : '离线' }}-->
                  </div>
                  <div slot="autoRegister"
                      slot-scope="text, record">
                    {{ record.autoRegister && record.autoRegister === true ? '自动' : '手动' }}
                  </div>

                  <div slot="operation"
                      slot-scope="text, record">
                    <div class="col-action" v-if="!record.autoRegister">
                      <a-popconfirm title="确定要删除吗？"
                                    okType="danger"
                                    ok-text="确定"
                                    cancel-text="取消"
                                    @confirm="artifactDispatchHandlerDelete(record)">
                        <a-button type="link"
                                  size="small">
                          <svg width="16"
                              height="16"
                              viewBox="0 0 20 20"
                              fill="none"
                              xmlns="http://www.w3.org/2000/svg">
                            <path class="fill-danger"
                                  fill-rule="evenodd"
                                  clip-rule="evenodd"
                                  d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
                                  fill="#111827" />
                          </svg>
                          <span class="text-danger">DELETE</span>
                        </a-button>
                      </a-popconfirm>
                      <a-button type="link"
                                size="small"
                                @click="artifactDispatchHandler(2, record)">
                        <svg width="16"
                            height="16"
                            viewBox="0 0 20 20"
                            fill="none"
                            xmlns="http://www.w3.org/2000/svg">
                          <path class="fill-muted"
                                d="M13.5858 3.58579C14.3668 2.80474 15.6332 2.80474 16.4142 3.58579C17.1953 4.36683 17.1953 5.63316 16.4142 6.41421L15.6213 7.20711L12.7929 4.37868L13.5858 3.58579Z"
                                fill="#111827" />
                          <path class="fill-muted"
                                d="M11.3787 5.79289L3 14.1716V17H5.82842L14.2071 8.62132L11.3787 5.79289Z"
                                fill="#111827" />
                        </svg>
                        <span class="text-dark">EDIT</span>
                      </a-button>
                    </div>
                  </div>
                </a-table>
              </a-card>
            </a-tab-pane>
            <a-tab-pane key="2"
              tab="外部节点">
              <ExternalNode/>
            </a-tab-pane>
        </a-tabs>
      </a-tab-pane>
      <a-tab-pane key="7"
                  tab="Webhook">
        <Webhook :activeKey="activeKey"></Webhook>
      </a-tab-pane>

    </a-tabs>
    <a-modal v-model="showMetadataHandler"
             :title="handlerMetadataType === 1 ? '新增元数据' : '修改元数据'"
             :maskClosable="false"
             cancelText="取消"
             okText="确定"
             @cancel="metadataHandlerCancel()"
             @ok="metadataHandlerConfirm()"
             centered>
      <a-form-model layout="horizontal"
                    ref="metadataForm"
                    :model="metadataForm"
                    :rules="metadataRules"
                    :hideRequiredMark="true">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="元数据KEY"
                               :colon="false"
                               prop="key">
              <a-input :disabled="handlerMetadataType !== 1"
                       placeholder="请输入元数据KEY"
                       v-model="metadataForm.key" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="元数据类型"
                               :colon="false"
                               prop="type">
              <a-select v-model="metadataForm.type"
                        placeholder="请选择元数据类型"
                        show-search
                        optionFilterProp="label">
                <a-select-option v-for="(item, index) in metadataTypes"
                                 :label="item.label"
                                 :key="index"
                                 :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="是否展示"
                               :colon="false"
                               prop="viewShow">
              <a-switch v-model="metadataForm.viewShow" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>
    <a-modal v-model="showVulnerabilitiesModal"
             :title="vulnerabilitiesType === 1 ? '添加白名单' : '添加黑名单'"
             :maskClosable="false"
             cancelText="取消"
             okText="确定"
             @cancel="vulnerabilitiesModalCancel()"
             @ok="addVulnerabilities()"
             centered>
      <a-input v-model="uuid"
               placeholder="请输入漏洞编号" />
    </a-modal>

    <AddPackageName v-if="showPackageNameModal" :modelVisible="showPackageNameModal"
      @packageNameHandlerCancel="packageNameModalCancel" @packageNameRefresh="packageNameRefresh" />

    <a-modal v-model="showArtifactDispatchHandler"
             :title="handlerArtifactDispatchType === 1 ? '新增分发配置' : '修改分发配置'"
             :maskClosable="false"
             cancelText="取消"
             okText="确定"
             @cancel="artifactDispatchHandlerCancel()"
             @ok="artifactDispatchHandlerConfirm()"
             centered>
      <a-form-model layout="horizontal"
                    ref="artifactDispatchForm"
                    :model="artifactDispatchForm"
                    :rules="artifactDispatchRules"
                    :hideRequiredMark="false">
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="集群节点英文名"
                               :colon="false"
                               prop="clusterEnName">
              <a-input :disabled="handlerArtifactDispatchType !== 1"
                       placeholder="请输入集群节点英文名"
                       v-model="artifactDispatchForm.clusterEnName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="集群中文名"
                               :colon="false"
                               prop="clusterCnName">
              <a-input placeholder="请输入集群中文名"
                       v-model="artifactDispatchForm.clusterCnName" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="集群节点地址"
                               :colon="false"
                               prop="clusterNodeHost">
              <a-input placeholder="请输入集群节点地址"
                       v-model="artifactDispatchForm.clusterNodeHost" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="集群描述"
                               :colon="false"
                               prop="clusterNodeDesc">
              <a-input placeholder="请输入描述"
                       v-model="artifactDispatchForm.clusterNodeDesc" />
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="分发类型"
                               :colon="false"
                               prop="dispatchType">
              <a-select v-model="artifactDispatchForm.dispatchType"
                        placeholder="请选择分发类型"
                        show-search
                        optionFilterProp="label">
                <a-select-option v-for="(item, index) in artifactDispatchTypes"
                                 :label="item.label"
                                 :key="index"
                                 :value="item.value">
                  {{ item.label }}
                </a-select-option>
              </a-select>
            </a-form-model-item>
          </a-col>
          <a-col :span="24">
            <a-form-model-item class="mb-10"
                               label="本集群"
                               :colon="false"
                               prop="isThisCluster">
              <a-switch v-model="artifactDispatchForm.isThisCluster" />
            </a-form-model-item>
          </a-col>
        </a-row>
      </a-form-model>
    </a-modal>

    <!--新增/编辑sso 对话框-->
    <a-modal v-model="ssoDialogShow"
             :title="ssoActionName"
             ok-text="保存"
             cancel-text="取消"
             @ok="handleOk"
             @cancel="handleCancel"
             width="60%">
      <a-form :form="ssoform"
              layout="vertical">
             
        <a-form-item >
          <span slot="label">
            clientId&nbsp;
            <a-tooltip title="客户端唯一标识,客户端id" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['clientId', { rules: [{ required: true, message: '请输入客户端ID!' }] }]"
                   placeholder="请填写客户端id"  :disabled="ssoActionName.includes('编辑')"/>
        </a-form-item>
        <a-form-item >
          <span slot="label">
            客户端名称&nbsp;
            <a-tooltip title="客户端名称" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['clientName', { rules: [{ required: true, message: '请输入客户端名称' }] }]"
                   placeholder="请填写客户端名称"  />
        </a-form-item>

        
        <a-form-item >
          <span slot="label">
            登录地址&nbsp;
            <a-tooltip title="单点登录页面所对应的url地址,由第三方登录系统提供!" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['ssoPath', { rules: [{ required: true, message: '请输入登录页面地址!' }] }]"
                   placeholder="请输入登录页面地址!" />
        </a-form-item>

        <a-form-item >
          <span slot="label">
            退出登录地址&nbsp;
            <a-tooltip title="单点登录页面退出登录所独调用的url地址,由第三方登录系统提供.用于清除服务端的会话!" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['loginOutUrl', { rules: [{ required: true, message: '请输入退出登录页面地址!' }] }]"
                   placeholder="请输入退出登录页面地址!" />
        </a-form-item>

        <a-form-item >
          <span slot="label">
            退出重定向url&nbsp;
            <a-tooltip title="退出登录后重定向地址！" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['loginOutRedPath', { rules: [{ required: true, message: '退出重定向url!' }] }]"
                   placeholder="退出重定向url!" />
        </a-form-item>
 
        <a-form-item >
          <span slot="label">
            登录重定向地址&nbsp;
            <a-tooltip title="登录后重定向地址！" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['redirectPath', { rules: [{ required: true, message: '请输入跳转路由!' }] }]"
                   placeholder="请输入跳转路由!" />
        </a-form-item>

        <a-form-item >
          <span slot="label">
            获取token接口地址&nbsp;
            <a-tooltip title="获取accessToken的接口地址！" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['access_token_url', { rules: [{ required: true, message: '请输入accessToken的接口地址!' }] }]"
                   placeholder="请输入accessToken的接口地址!" />
        </a-form-item>

        <a-form-item >
          <span slot="label">
            配置说明&nbsp;
            <a-tooltip title="描述信息" class="info-message">
              <a-icon type="question-circle-o" />
            </a-tooltip>
          </span>
          <a-input v-decorator="['desc', { rules: [{ required: true, message: '请输入配置说明!' }] }]"
                   placeholder="请输入配置说明!" />
        </a-form-item>

      </a-form>
    </a-modal>

  </div>

</template>

<script>
import {
  getServerSettings,
  postServerSettings,
  getLdap,
  putLdap,
  getMachineCode,
  postActivate,
  checkMachineCode,
  getMetadataConfiguration,
  globalSettingAddOrUpdateMetadata,
  globalSettingDeleteMetadata,
  getArtifactDispatchConfig,
  globalSettingArtifactDispatchConfig,
  globalSettingDelArtifactDispatchConfig
} from '@/api/settings'
import { getUsersCreateFields, getUsers } from '@/api/users'
import {
  addVulnerabilitiesWhite,
  addVulnerabilitiesBlack,
  removeVulnerabilitiesWhite,
  removeVulnerabilitiesBlack,
  saveOrUpdateVulnerabilityNotify,
  securityPolicyConfig,
  securityPolicyBlock,
  securityPolicyAddPackageName,
  securityPolicyDeletePackageName
} from '@/api/folib'

import {
  getSsoList,
  addSsoClient,
  updateSsoClient,
  deleteClient

} from '@/api/sso'
import Webhook from './components/Webhook/index.vue'
import ExternalNode from './components/ExternalNode/index.vue'
import {upperCase} from "@antv/util";
import PackageName from "./components/Package/index.vue"
import AddPackageName from "./components/Package/add.vue"

export default {
  props: ['navbarFixed'],
  components: {
    Webhook,
    ExternalNode,
    PackageName,
    AddPackageName
  },
  data() {
    return {
      step: 0,
      serverSettings: {
        instanceName: 'folib',
        baseUrl: 'http://localhost:38080/',
        port: 38080,
        kbps: 0,
        advancedConfigurationForm: {
          allowAnonymous: true,
          showChecksum: true,
        },
        corsConfigurationForm: { allowedOrigins: ['*'], corsAllowAll: false },
        smtpConfigurationForm: {
          host: null,
          port: null,
          username: null,
          password: null,
          connection: null
        },
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
        url: 'ldap://127.0.0.1:53389/dc=carlspring,dc=com',
        managerDn: 'cn=admin,dc=carlspring,dc=com',
        managerPassword: 'password',
        userPasswordEncoded: false,
        userSearchBase: 'ou=Users',
        userSearchFilter: '(uid={0})',
        roleMappingList: [{ externalRole: 'Admins', folibRole: 'ADMIN' }],
        userDnPatternList: ['uid={0},ou=Users'],
        enableProvider: false,
        authorities: {
          groupSearchBase: 'ou=Groups',
          groupSearchFilter: '(uniqueMember={0})',
          searchSubtree: true,
          groupRoleAttribute: 'cn',
          rolePrefix: '',
          convertToUpperCase: false
        }
      },
      machineInfo: { mac: null, haveError: true, dalyOut: true, object: null },
      activateCode: null,
      vulnerabilities: {
        whiteList: [],
        blackList: []
      },
      showVulnerabilitiesModal: false,
      vulnerabilitiesType: null,
      uuid: '',
      ruleForm: this.$form.createForm(this, { name: 'rule_form' }),
      blockForm: this.$form.createForm(this, { name: 'block_form' }),
      userList: [],
      artifactDispatchColumns: [
        {
          title: '集群节点英文名',
          dataIndex: 'clusterEnName',
          key: 'clusterEnName',
          width: 100
        },
        {
          title: '集群中文名',
          dataIndex: 'clusterCnName',
          key: 'clusterCnName',
          width: 100
        },
        {
          title: '节点',
          dataIndex: 'clusterNodeHost',
          key: 'clusterNodeHost',
          width: 200
        },
        {
          title: '描述',
          dataIndex: 'clusterNodeDesc',
          key: 'clusterNodeDesc',
          width: 100
        },
        {
          title: '分发方式',
          dataIndex: 'dispatchType',
          key: 'dispatchType',
          width: 80
        },
        {
          title: '本集群',
          dataIndex: 'isThisCluster',
          key: 'isThisCluster',
          width: 80,
          scopedSlots: { customRender: 'isThisCluster' }
        },
        {
          title: '在线状态',
          dataIndex: 'wsClientOnline',
          key: 'wsClientOnline',
          width: 80,
          scopedSlots: { customRender: 'wsClientOnline' }
        },
        {
          title: '添加方式',
          dataIndex: 'autoRegister',
          key: 'autoRegister',
          width: 80,
          scopedSlots: { customRender: 'autoRegister' }
        },
        {
          title: '操作',
          dataIndex: 'operation',
          width: 80,
          scopedSlots: { customRender: 'operation' }
        }
      ],
      metadataColumns: [
        {
          title: '元数据KEY',
          dataIndex: 'key',
          key: 'key',
          width: 200
        },
        {
          title: '元数据类型',
          dataIndex: 'type',
          key: 'type',
          width: 200,
          scopedSlots: { customRender: 'type' }
        },
        {
          title: '是否展示',
          dataIndex: 'viewShow',
          key: 'viewShow',
          width: 200,
          scopedSlots: { customRender: 'viewShow' }
        },
        {
          title: '操作',
          dataIndex: 'operation',
          width: 80,
          scopedSlots: { customRender: 'operation' }
        }
      ],
      metadataList: [],
      artifactDispatchList: [],
      showMetadataHandler: false,
      handlerMetadataType: 1,
      handlerArtifactDispatchType: 1,
      artifactDispatchForm: {
        clusterEnName: undefined,
        clusterCnName: undefined,
        clusterNodeDesc: undefined,
        clusterNodeHost: undefined,
        dispatchType: undefined,
        isThisCluster: undefined
      },
      metadataForm: {
        key: undefined,
        type: undefined,
        viewShow: false
      },
      artifactDispatchRules: {
        clusterEnName: [
          { required: true, message: '请输入集群节点英文名', trigger: 'blur' },
          { min: 1, max: 60, message: '长度在 1 到 60 个字符', trigger: 'blur' }
        ],
        clusterNodeHost: [
          { required: true, message: '请输入节点url', trigger: 'blur' }
        ],
        dispatchType: [
          { required: true, message: '请选择分发方式', trigger: 'blur' }
        ]
      },
      metadataRules: {
        key: [
          { required: true, message: '请输入元数据KEY', trigger: 'blur' },
          { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
        ],
        type: [{ required: true, message: '请选择元数据类型', trigger: 'blur' }]
      },
      artifactDispatchTypes: [
        {
          label: 'push',
          value: 'push'
        },
        {
          label: 'pull',
          value: 'pull'
        }
      ],
      metadataTypes: [
        {
          label: '数字',
          value: 'NUMERICAL'
        },
        {
          label: '字符串',
          value: 'STRING'
        },
        {
          label: '文本',
          value: 'TEXT'
        },
        {
          label: 'Markdown',
          value: 'MD'
        },
        {
          label: 'JSON',
          value: 'JSON'
        }
      ],
      packageNameShow: false,
      showPackageNameModal: false,
      activeKey: '1',
      showArtifactDispatchHandler: false,
      ssoform: this.$form.createForm(this, { name: 'ssoform' }),
      ssoColumns: [
      
        {
          title: '客户端id',
          dataIndex: 'clientId',
          key: 'clientId'
        },

        {
          title: '客户端名称',
          dataIndex: 'clientName',
          key: 'clientName'
        },
        
        {
          title: '重定向地址',
          dataIndex: 'redirectPath',
          key: 'redirectPath'
        },
        {
          title: '单点登录地址',
          key: 'ssoPath',
          dataIndex: 'ssoPath'
        },
        {
          title: '单点退出登录地址',
          key: 'loginOutUrl',
          dataIndex: 'loginOutUrl'
        },

        {
          title: '退出登录重定向地址',
          key: 'loginOutRedPath',
          dataIndex: 'loginOutRedPath'
        },     
        {
          title: '描述',
          key: 'desc',
          dataIndex: 'desc'
        },
        {
          title: '操作',
          dataIndex: 'operation',
          scopedSlots: { customRender: 'operation' }
        }
      ],
      instanceName:sessionStorage.getItem("instanceName")||"",
      ssoActionName: '',
      ssoDialogShow: false,
      ssoObj: {},
      ssoList: []
    }
  },
  computed: {},
  created() {
    this.getServerSettings()
    this.getLdap()
    this.getUsersCreateFields()
    this.getMachineCode()
    this.getVulnerabilities()
    this.getArtifactDispatchConfig()
    this.getSsoList()
  },
  methods: {
    upperCase,
    getSsoList(){
      getSsoList().then(res=>{
        this.ssoList=res
      })

    },
    handleSubmit(e) {
      e.preventDefault()
      this.form.validateFields((err, values) => {
        if (!err) {
        }
      })
    },
    tabChange(key) {
      this.activeKey = key
      if (key === '2') {
        this.getVulnerabilities()
      } else if (key === '5') {
        this.getMetadataConfiguration()
      }
    },
    moveStep(distance) {
      this.step += distance
    },
    getServerSettings() {
      getServerSettings().then(res => {
        this.serverSettings = res
        let allowedOrigins = this.serverSettings.corsConfigurationForm
          .allowedOrigins
        if (
          allowedOrigins &&
          allowedOrigins.length === 1 &&
          allowedOrigins[0] === '*'
        ) {
          this.serverSettings.corsConfigurationForm.corsAllowAll = true
        } else {
          this.serverSettings.corsConfigurationForm.corsAllowAll = false
        }
      })
    },
    saveServerSettings() {
      postServerSettings(this.serverSettings).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: '保存成功'
          })
        }, 100)
      })
    },
    handleClick(e, link) {
      e.preventDefault()
    },
    // Languages select field search method.
    filterOption(input, option) {
      return (
        option.componentOptions.children[0].text
          .toLowerCase()
          .indexOf(input.toLowerCase()) >= 0
      )
    },
    getLdap() {
      getLdap().then(res => {
        this.ldap = res
      })
    },
    putLdap() {
      putLdap(this.ldap).then(res => {
        setTimeout(() => {
          this.$notification.success({
            message: '保存成功'
          })
        }, 100)
      })
    },
    getUsersCreateFields() {
      getUsersCreateFields().then(res => {
        let roles = res.formDataValues[0].values
        let roleNameList = [
          'ADMIN',
          'GENERAL',
          'ARTIFACTS_MANAGER',
          'OPEN_SOURCE_MANAGE'
        ]
        roles = roles.filter(item => roleNameList.includes(item.name))
        this.assignableRoles = roles
      })
    },
    roleMappingDelHandle(item, index) {
      this.ldap.roleMappingList.splice(index, 1)
    },
    roleMappingAddHandle() {
      this.ldap.roleMappingList.splice(this.ldap.roleMappingList.length, 0, {
        externalRole: null,
        folibRole: null
      })
    },
    getMachineCode() {
      checkMachineCode().then(res => {
        this.machineInfo = res
      })
    },
    copy(code) {
      var input = document.createElement('input') // 创建input对象
      input.value = code // 设置复制内容
      document.body.appendChild(input) // 添加临时实例
      input.select() // 选择实例内容
      document.execCommand('Copy') // 执行复制
      document.body.removeChild(input) // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: '复制成功'
        })
      }, 100)
    },
    postActivate(isPoc) {
      if (this.activateCode) {
        postActivate(this.activateCode, isPoc).then(res => {
          if (res.rel) {
            setTimeout(() => {
              this.$notification.success({
                message: '激活成功'
              })
              this.getMachineCode()
            }, 100)
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: '激活失败',
                description: res.message
              })
            }, 100)
          }
        })
      } else {
        setTimeout(() => {
          this.$notification.open({
            class: 'ant-notification-warning',
            message: '无法激活',
            description: '没有输入序列号'
          })
        }, 100)
      }
    },
    getVulnerabilities(type) {
      securityPolicyConfig()
        .then(res => {
          if (type !== 2) {
            this.vulnerabilities.whiteList = res.whites
          }
          if (type !== 1) {
            this.vulnerabilities.blackList = res.blacks
          }
        })
        .finally(() => {})
    },
    removeWhite(uuid) {
      removeVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + '从白名单移除成功')
        })
        .finally(() => {
          this.getVulnerabilities(1)
        })
    },
    removeBlack(uuid) {
      removeVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + '从黑名单移除成功')
        })
        .finally(() => {
          this.getVulnerabilities(2)
        })
    },
    addWhite(uuid) {
      addVulnerabilitiesWhite({ white: uuid })
        .then(res => {
          this.successMsg(uuid + '添加到白名单成功')
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.uuid = ''
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(1)
        })
    },
    addBlack(uuid) {
      addVulnerabilitiesBlack({ black: uuid })
        .then(res => {
          this.successMsg(uuid + '添加到黑名单成功')
        })
        .catch(err => {
          this.$notification['error']({
            message: err.response.data.error,
            description: ''
          })
        })
        .finally(() => {
          this.uuid = ''
          this.showVulnerabilitiesModal = false
          this.getVulnerabilities(2)
        })
    },
    vulnerabilitiesModalCancel() {
      this.showVulnerabilitiesModal = false
    },
    addVulnerabilities() {
      if (!this.uuid) {
        this.$notification['warning']({
          message: '请输入漏洞编号',
          description: ''
        })
        return
      }
      if (this.vulnerabilitiesType === 1) {
        this.addWhite(this.uuid)
      } else if (this.vulnerabilitiesType === 2) {
        this.addBlack(this.uuid)
      }
    },
    successMsg(message) {
      if (!message) {
        message = '操作成功'
      }
      this.$notification['success']({
        message: message,
        description: ''
      })
    },
    getSecurityPolicy() {
      securityPolicyConfig()
        .then(res => {
          this.$nextTick(() => {
            if (this.$refs.ruleForm) {
              this.ruleForm.setFieldsValue({
                levels: res.levels,
                notifyScopes: res.notifyScopes,
                receiverUsers: res.receiverUsers,
                receiverEmails: res.receiverEmails
              })
            }
            if (this.$refs.blockForm) {
              this.blockForm.setFieldsValue({
                blockType: res.blockType,
                blockLevels: res.blockLevels,
                filterWhites: res.filterWhites
              })
              if (res.blockType === 3) {
                this.packageNameShow = true
              } else {
                this.packageNameShow = false
              }
            }
          })
        })
        .finally(() => {})
    },
    vulnerabilityTabChange(key) {
      if (key === '3') {
        this.getSecurityPolicy()
        this.getUsersList()
      } else if (key === '4') {
        this.getSecurityPolicy()
      } else {
        this.getVulnerabilities()
      }
    },
    ruleFormSubmit(e) {
      e.preventDefault()
      this.ruleForm.validateFieldsAndScroll((err, values) => {
        if (!err) {
          saveOrUpdateVulnerabilityNotify(values)
            .then(res => {
              this.successMsg('通知设置保存成功')
              this.getSecurityPolicy()
            })
            .finally(() => {})
        }
      })
    },
    ruleFormCancel() {
      this.ruleForm.resetFields()
      this.getSecurityPolicy()
    },
    getUsersList() {
      getUsers().then(res => {
        this.userList = res.users
      })
    },
    filterOption(input, option) {
      return (
        option.componentOptions.children[0].text
          .toLowerCase()
          .indexOf(input.toLowerCase()) >= 0
      )
    },
    blockFormSubmit(e) {
      e.preventDefault()
      this.blockForm.validateFieldsAndScroll((err, values) => {
        if (!err) {
          securityPolicyBlock(values)
            .then(res => {
              this.successMsg('阻断设置保存成功')
              this.getSecurityPolicy()
            })
            .finally(() => {})
        }
      })
    },
    blockFormCancel() {
      this.blockForm.resetFields()
      this.getSecurityPolicy()
    },
    blockTypeChange(blockType) {
      if (blockType !== 1) {
        this.blockForm.setFieldsValue({
          blockLevels: [],
          filterWhites: false
        })
      }
      this.packageNameShow = false
      if (blockType === 3) {
        this.packageNameShow = true
      }
    },
    getMetadataConfiguration() {
      getMetadataConfiguration()
        .then(res => {
          this.metadataList = res
        })
        .finally(() => {})
    },
    artifactDispatchFormRest() {
      if (this.$refs.artifactDispatchForm) {
        this.$refs.artifactDispatchForm.resetFields()
      }
      this.artifactDispatchForm = {
        clusterEnName: undefined,
        clusterCnName: undefined,
        clusterNodeDesc: undefined,
        clusterNodeHost: undefined,
        dispatchType: undefined,
        isThisCluster: undefined
      }
    },
    metadataFormReset() {
      if (this.$refs.metadataForm) {
        this.$refs.metadataForm.resetFields()
      }
      this.metadataForm = {
        key: undefined,
        type: undefined,
        viewShow: false
      }
    },
    metadataHandler(type, item) {
      this.metadataFormReset()
      if (item) {
        let data = Object.assign({}, item)
        if (data.viewShow === 1) {
          data.viewShow = true
        } else {
          data.viewShow = false
        }
        this.metadataForm = data
      }
      this.handlerMetadataType = type
      this.showMetadataHandler = true
    },
    artifactDispatchHandlerConfirm() {
      this.$refs.artifactDispatchForm.validate(valid => {
        if (valid) {
          let data = Object.assign({}, this.artifactDispatchForm)
          if (this.handlerArtifactDispatchType === 1) {
            let flag = this.artifactDispatchList.some(
              x => x.clusterEnName === data.clusterEnName
            )
            if (flag) {
              this.$notification['warning']({
                message: '集群分发配置已存在',
                description: ''
              })
              return false
            }
          }
          globalSettingArtifactDispatchConfig(data)
            .then(res => {
              let prefix = '新增'
              if (this.handlerArtifactDispatchType === 2) {
                prefix = '修改'
              }
              this.successMsg(prefix + '分发配置成功')
              this.artifactDispatchFormRest()
              this.showArtifactDispatchHandler = false
              this.getArtifactDispatchConfig()
            })
            .finally(() => {})
        } else {
          return false
        }
      })
    },
    metadataHandlerConfirm() {
      this.$refs.metadataForm.validate(valid => {
        if (valid) {
          let data = Object.assign({}, this.metadataForm)
          if (this.handlerMetadataType === 1) {
            let flag = this.metadataList.some(
              metadata => metadata.key === data.key
            )
            if (flag) {
              this.$notification['warning']({
                message: '元数据KEY已存在',
                description: ''
              })
              return false
            }
          }
          if (data.viewShow) {
            data.viewShow = 1
          } else {
            data.viewShow = 0
          }
          globalSettingAddOrUpdateMetadata(data)
            .then(res => {
              let prefix = '新增'
              if (this.handlerMetadataType === 2) {
                prefix = '修改'
              }
              this.successMsg(prefix + '元数据配置成功')
              this.metadataFormReset()
              this.showMetadataHandler = false
              this.getMetadataConfiguration()
            })
            .finally(() => {})
        } else {
          return false
        }
      })
    },
    artifactDispatchHandlerDelete(data) {
      globalSettingDelArtifactDispatchConfig(data.clusterEnName)
        .then(res => {
          this.successMsg('删除分发配置成功')
        })
        .finally(() => {
          this.getArtifactDispatchConfig()
        })
    },
    metadataHandlerDelete(data) {
      globalSettingDeleteMetadata(data)
        .then(res => {
          this.successMsg('删除元数据成功')
        })
        .finally(() => {
          this.getMetadataConfiguration()
        })
    },
    getArtifactDispatchConfig() {
      getArtifactDispatchConfig().then(res => {
        this.artifactDispatchList = res
      })
    },
    artifactDispatchHandler(type, item) {
      this.artifactDispatchFormRest()
      if (item) {
        let data = Object.assign({}, item)
        this.artifactDispatchForm = data
      }
      this.handlerArtifactDispatchType = type
      this.showArtifactDispatchHandler = true
    },

    metadataHandlerCancel() {
      this.metadataFormReset()
      this.showMetadataHandler = false
    },
    artifactDispatchHandlerCancel() {
      this.artifactDispatchFormRest()
      this.showArtifactDispatchHandler = false
    },
    packageNameModalCancel() {
      this.showPackageNameModal = false
    },
    packageNameModalShow() {
      this.showPackageNameModal = true
    }, 
    allowedOriginsChange() {
      let allowedOrigins = this.serverSettings.corsConfigurationForm
        .allowedOrigins
      if (
        allowedOrigins &&
        allowedOrigins.length === 1 &&
        allowedOrigins[0] === '*'
      ) {
        this.serverSettings.corsConfigurationForm.corsAllowAll = true
      } else {
        this.serverSettings.corsConfigurationForm.corsAllowAll = false
      }
    },
    corsAllowAllChange(val) {
      if (val) {
        this.serverSettings.corsConfigurationForm.allowedOrigins = ['*']
      }
    },
    /**
     * 编辑单点登录客户端
     */
    ssoEdit(val) {
      this.ssoActionName = '编辑客户端'
      this.$nextTick(() => {
        this.ssoform.setFieldsValue({ ...val}) // loadsh的pick方     
      })
      this.ssoDialogShow = true
     
    },
    /**
     * 删除单点登录客户端
     */
    ssoDelete(val) {
      deleteClient({clientId:val.clientId}).then(()=>{
        this.$notification.success({
              message: "提示",
              description: "删除客户端成功",
            })   
            this.getSsoList()
      }).catch((err) => {
        this.$notification["error"]({
          message: err.response.data.error,
        })
      })
    },
    /**
     * 新增单点登录客户端
     */
    ssoAdd() {
      this.ssoActionName = '新增客户端'
      this.ssoObj = {    
        clientId: '',
        redirectPath: '',
        ssoPath: '',
        desc: '',
        picIcoin: '' ,
        loginOutRedPath:'',
        loginOutUrl:'',
        clientName:'',
        access_token_url:''
      }
      this.$nextTick(() => {
        this.ssoform.setFieldsValue({...this.ssoObj}) 
        this.ssoDialogShow = true
      })
     
    },
    /**
     * 提交客户端信息
     */
    handleOk() {
      
      // 校验
      this.ssoform.validateFields((err,fieldsValue) => {
       
        if (!err) {
          // 新增逻辑
          if(this.ssoActionName.includes("新增")){
          addSsoClient(fieldsValue).then(()=>{   
            this.$notification.success({
              message: "提示",
              description: "新增客户端成功",
            })   
            this.ssoDialogShow=false     
            this.getSsoList()
          }).catch((err) => {
            this.$notification["error"]({
              message: err.response.data.error,
            })
          })       
        }else{
          updateSsoClient(fieldsValue).then(()=>{   
            this.$notification.success({
              message: "提示",
              description: "更新客户端成功",
            })   
            this.ssoDialogShow=false     
            this.getSsoList()
          }).catch((err) => {
            this.$notification["error"]({
              message: err.response.data.error,
            })
          })
        }
      }
      })
    },
    /**
     * 关闭对话框
     */
    handleCancel() {
      this.ssoDialogShow = false
    },
    packageNameRefresh() {
      this.$refs.packageName.getPackageNameList()
    },
  }
}
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

    .ant-anchor-link-title-active {
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
    border-left: 1px solid hsla(0, 0%, 100%, 0.3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: 0.75;
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
      border-left: 1px solid hsla(0, 0%, 100%, 0.3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: 0.75;

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

  .white-group,
  .black-group {
    width: 100%;
    display: inline-flex;
    justify-content: flex-start;
    flex-wrap: wrap;
  }

  .white-group .white,
  .black-group .black {
    margin-right: 10px;
    margin-bottom: 10px;
    width: calc((100% - 50px) / 5);
  }

  // .white-card,.black-card{
  //   height:100%;
  //   margin-right: 10px;
  //   width: calc((100% - 20px) / 2);
  //   overflow-y: auto;
  // }
  .white-group .uuid,
  .black-group .uuid {
    font-size: 5px;
  }

  .d-popconfirm {
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
    background-color: #1890ff;
    border-radius: 8px;
    display: inline-flex;
    justify-content: center;
    align-items: center;
  }

  .o-btn img {
    width: 20px;
    height: 20px;
    cursor: pointer;
  }

  .o-black {
    background-color: #f58080;
  }

  .ant-pagination-prev .ant-pagination-item-link,
  .ant-pagination-next .ant-pagination-item-link {
    margin-top: 2.5px;
  }

  .ant-pagination-item {
    margin-top: 5px;
  }

  .block-full {
    padding: 0 0 0 25px;
  }

  .block-form .block-full .ant-form-item-label,
  .block-form .block-full .ant-checkbox-group,
  .block-form .block-full .ant-form-item-control {
    line-height: 36px;
  }

  .block-form .block-full .ant-form-item {
    margin-bottom: unset;
  }

  .block-form .tips {
    color: grey;
    font-size: 12px;
    opacity: 0.7;
  }

  .cursor-pointer {
    cursor: pointer;
  }

  .package-name-add {
    vertical-align: middle;
  }

  .block-settings,
  .package-name-list.ant-card {
    box-shadow: unset;
  }
  .block-settings,
  .package-name-list.ant-card-bordered {
    border: unset;
  }
}
.info-message{
  color:#fff;
  background:#1890ff ;
  border-radius: 50%;
  scale: (1.3);
}
</style>
