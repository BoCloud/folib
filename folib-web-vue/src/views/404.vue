<!-- 
	This is the 404 error page, it uses the dashboard layout in: 
	"./layouts/Default.vue" .
 -->

<template>
	<a-result status="404" title="404" sub-title="你需要访问的页面不存在.">
		<template #extra>
			<router-link class="ant-btn ant-btn-primary" to="/">返回 登录页</router-link>
		</template>
	</a-result>
</template>