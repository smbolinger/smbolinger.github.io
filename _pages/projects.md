---
title: Projects
permalink: /projects/
homeex: true
---

<!-- <h2>My Projects</h2> -->
<!-- <ul> -->
<!--   {% for project in site.portfolio %} -->
<!--     <li> -->
<!--       <strong>{{ project.title }}</strong> -  -->
<!--       <a href="{{ project.url | relative_url }}">View Details</a> -->
<!---->
<!--       {% if project.github_repo %} -->
<!--         | <a href="https://github.com/{{ project.github_repo }}" target="_blank" rel="noopener"> -->
<!--             View on GitHub -->
<!--           </a> -->
<!--       {% endif %} -->
<!--     </li> -->
<!--   {% endfor %} -->
<!-- </ul> -->

<div class="repo-grid">
  {% for project in site.portfolio %}
    <div class="repo-tile">
      
      {% if project.preview_image %}
        <div class="tile-image-wrapper">
          <img src="{{ project.header.teaser | relative_url }}" alt="{{ project.title }} Preview" class="tile-image">
        </div>
      {% endif %}
      
      <div class="tile-content">
        <h3>{{ project.title }}</h3>
        
        <p class="tile-description">
          {% if project.excerpt %}
            {{ project.excerpt }}
          {% else %}
            {{ project.content | strip_html | truncatewords: 50 }}
          {% endif %}
        </p>
        
        <!-- Action Links -->
        <div class="tile-links">
          {% if project.link_page %}
          <a href="{{ project.url | relative_url }}" class="btn btn-secondary">Read More</a>
          {% endif %}
          {% if project.github_repo %}
            <a href="https://github.com{{ project.github_repo }}" target="_blank" rel="noopener" class="btn btn-primary">
              View on GitHub
            </a>
          {% endif %}
        </div>
      </div>
      
    </div>
  {% endfor %}
</div>

