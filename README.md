# Eye Movement Consistency Extended

## Summary
This repository contains the analysis scripts developed to analyze the study dataset. These scripts were based on the <code>ConsistentEyeMovement.R</code> written by Dr. Steven Luke for the [Eye Movement Consistency Study](http://web.b.ebscohost.com/ehost/pdfviewer/pdfviewer?vid=1&sid=a79bf3f3-f687-4769-b60d-7d4eb6fd8426%40pdc-v-sessmgr06 "Link to article") and it is contained in the <code>legacyFiles</code> directory.

This study is an extension of the Eye Movement Consistency study (now published as [Carter and Luke, 2018](http://web.b.ebscohost.com/ehost/pdfviewer/pdfviewer?vid=1&sid=a79bf3f3-f687-4769-b60d-7d4eb6fd8426%40pdc-v-sessmgr06 "Link to article")). This continues to address the question of whether and to what degree eye movements can be considered stable in an individual over time. This study adds additional knowledge to answer this question through the use of addtional tasks (reading, visual search and antisaccade) and increased testing frequency. 

## Methods Outline

<ol>
	<li>Data were initially viewed via DataViewer. Fixation and Saccade reports were created for all sessions, named conditionReportType.</li>
	<li>The script <code>analysis.R</code> performed the following functions:
	<ul>
	  <li>Preprocessing</li>
	  <li>Computation of participant means and standard deviations by task and session.</li>
	  <li>Correlation of means and standard deviation across tasks and sessions.</li>
	  <li>LMER</li>
	</ul>
</ol>
